library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(bslib)
library(DT)
library(leaflet.extras)
library(geojsonsf)
library(aws.s3)

# Turn off color contrast warnings
options(bslib.color_contrast_warnings = FALSE)

sf_use_s2(FALSE) # Disable S2 geometry for sf package

# Set AWS credentials
bucket_name <- "survey-polygons"
aws_region <- "ap-southeast-2"

AWS_credentials <- read.csv("s3-poly-access_accessKeys.csv") # nolint: object_name_linter, line_length_linter.

Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_credentials$Access.key.ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_credentials$Secret.access.key,
           "AWS_DEFAULT_REGION" = aws_region)

# Create a tmp folder if not exists
if (!dir.exists("input")) {
  dir.create("input")
}
# Unzip the CPES shapefile to the tmp folder
unzip("Data/CPES.zip", exdir = "input")

# Load the shapefile
CPES <- st_read("input/CPES") # nolint: object_name_linter.
CPES <- st_transform(CPES, 4326) # Set CRS to WGS84 # nolint
filtered <- CPES %>% filter(enterprise == "Agricultural")

# Define UI
ui <- fluidPage(
  div(
    style = "display: flex; align-items: center;",
    titlePanel("🌱 Planfarm TerraWise Pick Your Boudnary"),
    a(
      href = "instructions.pdf",  # Link to the help page
      target = "_blank",  # Open the link in a new tab
      tooltip(
        icon("circle-question",
             style = "margin-left: 10px; font-size: 1.5em;"),
        "Click here to download the instructions",
        placement = "auto"
      )  # Add spacing and size # nolint: line_length_linter.
    ),
    div(
      style = "margin-left: auto; display: flex; gap: 10px; height: 80px;",
      img(src = "logo/TerraWise_Med.png"),
      img(src = "logo/Planfarm_Icon.png")
    )
  ),

  br(),

  fluidRow(
    column(
      3,
      div(
        style = "background-color: #E2F0D9; 
        padding: 15px; border-radius: 20px;",
        textInput(
          "business_name", "Enter your business name:",
          placeholder = "Business Name", width = "100%"
        ),
        selectizeInput(
          "lga_selection", "Select your shire:",
          choices = unique(filtered$lga),
          multiple = TRUE, width = "100%"
        ),
        actionButton("add_to_selection", "Add Selected Property to Table",
                     icon = icon("plus"), class = "btn-secondary btn-block",
                     style = "margin-top: 15px; color: white;", width = "100%"),
        actionButton("clear_selection", "Clear Selected Property(s)",
                     icon = icon("trash"), class = "btn-danger btn-block",
                     style = "margin-top: 10px;", width = "100%"),
        actionButton("clear_selections", "Clear All Selections",
                     icon = icon("explosion"), class = "btn-danger btn-block",
                     style = "margin-top: 10px;", width = "100%")
      ),

      br(),

      actionButton("send_btn", "Submit boundaries to TerraWise",
                   icon = icon("arrow-up-from-bracket"),
                   class = "btn-primary btn-block",
                   style = "margin-top: 15px; color: white;", width = "100%")
    ),
    column(
      9,
      leafletOutput("map", height = "500px"),
      br(),
      card(
        card_header("Selected Properties"),
        DTOutput("table"), height = "250px"
      )
    )
  ),

  theme = bs_theme(brand = TRUE)
)

server <- function(input, output, session) {
  # Reactive value to store manually selected polygons
  selected_polygon <- reactiveVal(NULL)

  selected_data <- reactiveVal(data.frame(
    id = integer(),
    .nonce = numeric(),
    lat = numeric(),
    lng = numeric(),
    stringsAsFactors = FALSE
  ))

  # Filter data based on LGA selection
  subset_cpes <- reactive({
    if (length(input$lga_selection) == 0) {
      return(filtered[0, ])  # Empty dataframe with same structure
    }
    filtered %>% filter(lga %in% input$lga_selection)
  })

  # Get data for the table based on manually selected polygons
  table_data <- reactiveVal(NULL)

  # Calculate map center
  map_center <- reactive({
    subset <- subset_cpes()

    if (nrow(subset) == 0 || length(input$lga_selection) == 0) {
      # Default center if no selection (Center of WA)
      return(c(-28, 121))
    }

    # Calculate centroid for each LGA and average them
    centroids <- lapply(input$lga_selection, function(lga_name) {
      poly <- subset %>% filter(lga == lga_name)
      if (nrow(poly) > 0) {
        centroid <- st_centroid(st_union(poly$geometry))
        st_coordinates(centroid)
      }
    })

    # Filter out NULLs and calculate average
    centroids <- centroids[!sapply(centroids, is.null)]

    # Extract and average coordinates
    centroid_matrix <- do.call(rbind, centroids)
    avg_lon <- mean(centroid_matrix[, 1])
    avg_lat <- mean(centroid_matrix[, 2])

    return(c(avg_lat, avg_lon))
  })

  # Render leaflet map
  output$map <- renderLeaflet({
    # Initialize the map
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = 121.0, lat = -28.0, zoom = 5) %>%
      addSearchOSM(
        options = searchOptions(
          autoCollapse = TRUE,
          minLength = 2,
          zoom = 10
        )
      )
  })

  # Observe LGA selection and update map
  observeEvent(input$lga_selection, {
    center <- map_center()
    subset <- subset_cpes()

    if (nrow(subset) > 0) {
      # Create popup content
      popup_content <- paste0(
        "<b>Object id:</b> ", subset$oid_1,
        "<br><b>Property area:</b> ", subset$property_a,
        "<br><b>Enterprise: </b> ", subset$enterprise
      )

      # Add subsetted polygons to the map
      leafletProxy("map") %>%
        setView(lng = center[2], lat = center[1], zoom = 8) %>%
        addPolygons(
          data = subset, fillColor = "#D93026",
          weight = 2, opacity = 1, layerId = ~oid_1,
          color = "#92182B", fillOpacity = 0.4,
          popup = popup_content, popupOptions = popupOptions(closeOnClick = TRUE), # nolint: line_length_linter.
          highlight = highlightOptions(
            weight = 4,
            color = "#E2E3E4",
            fillOpacity = 0.7
          )
        )
    }
  })

  # Add highlighting when a polygon is clicked
  observeEvent(input$map_shape_click, {
    selected_polygon(input$map_shape_click)

    # Update the map to highlight the selected polygon
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      clearSearchOSM() %>%
      addPolygons(
        data = filtered %>% filter(oid_1 == selected_polygon()$id),
        fillColor = "yellow", weight = 4, opacity = 1,
        color = "#FB8D33", fillOpacity = 0.7, group = "highlight",
        options = pathOptions(clickable = FALSE)
      )
  })

  # Add selected polygon to the selection
  observeEvent(input$add_to_selection, {
    selected_id <- selected_polygon()$id

    if (!is.null(selected_id)) {

      # Only add if not already in the list
      if (!selected_id %in% selected_data()$id) {
        selected_data(rbind(selected_polygon(), selected_data()))

        # Show success notification
        showNotification(
          paste("Added polygon", selected_id, "to selection"),
          type = "message",
          duration = 3
        )

        # Update the map to show selected polygon in blue
        leafletProxy("map") %>%
          clearGroup("highlight") %>%
          addPolygons(
            data = filtered %>% filter(oid_1 %in% selected_data()$id),
            fillColor = "#19B4CA", weight = 3, opacity = 1,
            color = "#009CB2", fillOpacity = 0.6,
            group = as.character(selected_id),
            options = pathOptions(interactive = FALSE)
          )
        table_data(filter(filtered, oid_1 %in% selected_data()$id))
      } else {
        showNotification("This polygon is already selected", type = "warning", duration = 3) # nolint: line_length_linter.
      }
    } else {
      showNotification("No polygon selected", type = "warning", duration = 3)
    }
  })

  # Clear selected boundaries
  observeEvent(input$clear_selection, {
    req(input$table_rows_selected)
    row <- input$table_rows_selected

    for (i in row) {
      leafletProxy("map") %>%
        clearGroup(table_data()[i, ]$oid_1)

      table_data(table_data()[-i, ])
      selected_data(selected_data()[-i, ])
    }
  })

  # Clear all selections
  observeEvent(input$clear_selections, {
    req(table_data())
    showModal(
      modalDialog(
        "Are you sure you want to clear all selections?",
        title = "Clear All Selections",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_clear", "Clear All", class = "btn-danger")
        ),
        easyClose = TRUE
      )
    )
  })

  observeEvent(input$confirm_clear, {
    removeModal()
    selected_polygon(NULL)
    selected_data(data.frame(
      id = integer(),
      .nonce = numeric(),
      lat = numeric(),
      lng = numeric(),
      stringsAsFactors = FALSE
    ))

    to_be_removed <- table_data()$oid_1

    table_data(NULL)

    # Clear highlighting and selection on map
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      clearGroup(as.character(to_be_removed))

    # Show notification
    showNotification("All selections cleared", type = "message", duration = 3)
  })

  # Render data table
  output$table <- renderDT({
    subset <- table_data()
    if (is.null(subset)) {
      return()
    }

    # Select columns and convert to dataframe
    df <- subset %>%
      st_drop_geometry() %>%
      select(oid_1:lga)

    if ("property_n" %in% colnames(df)) {
      df <- df %>% select(-property_n)
    }

    datatable(df, options = list(pageLength = 10, searching = TRUE),
              selection = "multiple", rownames = FALSE)
  })

  # Download handler for selected properties
  observeEvent(input$send_btn, {
    if (nrow(table_data()) == 0) {
      showNotification("No properties selected", type = "warning", duration = 3)
      return()
    }

    # Create a temporary file to store GeoJSON data
    temp_file <- tempfile(fileext = ".geojson")
    # Convert selected data to GeoJSON
    geojson <- sf_geojson(table_data())
    # Save the GeoJSON to a file
    writeLines(geojson, temp_file)

    # Define file name for saving
    file_name <- paste0(
      "farm-boundary/", input$business_name,
      "_planting.geojson"
    )

    tryCatch({
      # Upload the GeoJSON file to S3
      put_object(
        file = temp_file, object = file_name,
        bucket = bucket_name, acl = "public-read"
      )

      showModal(
        modalDialog(
          title = "Success",
          "Your data has been successfully uploaded to TerraWise.",
          easyClose = FALSE,
          footer = NULL
        )
      )

      # Clear drawn polygons from map
      leafletProxy("map") %>% clearGroup("selected")

    }, error = function(e) {
      # Handle any errors that occur during file upload
      showNotification(
        HTML(paste0("<span style='color: red; font-weight: bold;'>❌ Error saving data: ", e$message, "</span>")) # nolint: line_length_linter.
      )
    })

    unlink(temp_file)  # Delete the temporary file after use
  })
}

shinyApp(ui, server)