library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(bslib)
library(DT)
library(leaflet.extras)

# Create a tmp folder if not exists
if (!dir.exists("input")) {
  dir.create("input")
}
# Unzip the CPES shapefile to the tmp folder
unzip("Data/CPES.zip", exdir = "input")

# Load the shapefile
CPES <- st_read("input/CPES")
CPES <- st_transform(CPES, 4326) # Set CRS to WGS84
filtered <- CPES %>% filter(enterprise == "Agricultural")

# Delete the tmp folder
unlink("input/", recursive = TRUE)

# Define UI
ui <- page_sidebar(
  title = "LGA Map Viewer",
  sidebar = sidebar(
    selectizeInput(
      "lga_selection",
      "Select your shire:",
      choices = unique(filtered$lga),
      multiple = TRUE
    ),
    textOutput("selected_polygon_info"),
    br(),
    actionButton("add_to_selection", "Add Selected Polygon to Table", 
                 icon = icon("plus"), 
                 class = "btn-primary btn-block",
                 style = "margin-top: 15px;"),
    actionButton("clear_selection", "Clear All Selections", 
                 icon = icon("trash"), 
                 class = "btn-danger btn-block",
                 style = "margin-top: 10px;"),
    br(),
    downloadButton("download_data", "Download Selected Properties", 
                  class = "btn-success btn-block",
                  style = "margin-top: 15px;")
  ),
  
  leafletOutput("map", height = 500),
  card(
    card_header("Selected Properties"),
    DTOutput("table")
  )
)

server <- function(input, output, session) {
  # Reactive value for selected polygon
  selected_polygon_id <- reactiveVal(NULL)
  
  # Reactive value to store manually selected polygons
  selected_polygons <- reactiveVal(character(0))
  
  # Filter data based on LGA selection
  subset_cpes <- reactive({
    if (length(input$lga_selection) == 0) {
      return(filtered[0,])  # Empty dataframe with same structure
    }
    filtered %>% filter(lga %in% input$lga_selection)
  })
  
  # Get data for the table based on manually selected polygons
  table_data <- reactive({
    selected_ids <- selected_polygons()
    if (length(selected_ids) == 0) {
      return(filtered[0,])
    }
    filtered %>% filter(oid_1 %in% selected_ids)
  })
  
  # Calculate map center
  map_center <- reactive({
    subset <- subset_cpes()
    
    if (nrow(subset) == 0 || length(input$lga_selection) == 0) {
      # Default center if no selection (Center of Australia)
      return(c(-25.2744, 133.7751)) 
    }
    
    # Calculate centroid for each LGA and average them
    centroids <- lapply(input$lga_selection, function(lga_name) {
      poly <- subset %>% filter(lga == lga_name)
      if (nrow(poly) > 0) {
        centroid <- st_centroid(st_union(poly$geometry))
        centroid_coords <- st_coordinates(centroid)
        return(centroid_coords)
      }
      return(NULL)
    })
    
    # Filter out NULLs and calculate average
    centroids <- centroids[!sapply(centroids, is.null)]
    
    if (length(centroids) == 0) {
      return(c(-25.2744, 133.7751))  # Default center
    }
    
    # Extract and average coordinates
    centroid_matrix <- do.call(rbind, centroids)
    avg_lon <- mean(centroid_matrix[, 1])
    avg_lat <- mean(centroid_matrix[, 2])
    
    return(c(avg_lat, avg_lon))
  })
  
  # Display information about selected polygon
  output$selected_polygon_info <- renderText({
    selected_id <- selected_polygon_id()
    if (is.null(selected_id)) {
      return("No polygon selected")
    }
    
    # Find the polygon in the full dataset
    row <- filtered %>% filter(oid_1 == selected_id)
    
    if (nrow(row) == 0) {
      return(paste("Selected ID:", selected_id, "(details not available)"))
    }
    
    paste0("Selected: Object ", row$oid_1, " - ", row$lga, " - Area: ", row$property_a)
  })
  
  # Render leaflet map
  output$map <- renderLeaflet({
    center <- map_center()
    subset <- subset_cpes()
    
    # Initialize the map
    m <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lng = center[2], lat = center[1], zoom = 9) %>%
      addSearchOSM(
        options = searchOptions(
          autoCollapse = TRUE,
          minLength = 2,
          zoom = 10
        )
      )
    
    # Add polygons to the map
    if (nrow(subset) > 0) {
      # Create popup content
      popup_content <- paste0(
        "<b>Object id:</b> ", subset$oid_1,
        "<br><b>Property area:</b> ", subset$property_a,
        "<br><b>Enterprise: </b> ", subset$enterprise
      )
      
      # Add polygons with click event
      m <- m %>%
        addPolygons(
          data = subset,
          fillColor = "red",
          weight = 2,
          opacity = 1,
          color = "red",
          fillOpacity = 0.4,
          popup = popup_content,
          layerId = ~oid_1,
          highlight = highlightOptions(
            weight = 4,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        )
      
      # Add already selected polygons with different color
      selected_ids <- selected_polygons()
      if (length(selected_ids) > 0) {
        selected_data <- subset %>% filter(oid_1 %in% selected_ids)
        if (nrow(selected_data) > 0) {
          m <- m %>%
            addPolygons(
              data = selected_data,
              fillColor = "blue",
              weight = 3,
              opacity = 1,
              color = "darkblue",
              fillOpacity = 0.6,
              popup = popup_content,
              group = "selected",
              options = pathOptions(clickable = FALSE)
            )
        }
      }
    }
    
    m
  })
  
  # Add highlighting when a polygon is clicked
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    selected_polygon_id(click$id)
    
    # Update the map to highlight the selected polygon
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = filtered %>% filter(oid_1 == click$id),
        fillColor = "yellow",
        weight = 4,
        opacity = 1,
        color = "orange",
        fillOpacity = 0.7,
        group = "highlight",
        options = pathOptions(clickable = FALSE)
      )
  })
  
  # Add selected polygon to the selection
  observeEvent(input$add_to_selection, {
    selected_id <- selected_polygon_id()
    
    if (!is.null(selected_id)) {
      current_selections <- selected_polygons()
      
      # Only add if not already in the list
      if (!selected_id %in% current_selections) {
        selected_polygons(c(current_selections, selected_id))
        
        # Show success notification
        showNotification(
          paste("Added polygon", selected_id, "to selection"),
          type = "message",
          duration = 3
        )
        
        # Update the map to show selected polygon in blue
        leafletProxy("map") %>%
          clearGroup("selected") %>%
          addPolygons(
            data = filtered %>% filter(oid_1 %in% selected_polygons()),
            fillColor = "blue",
            weight = 3,
            opacity = 1,
            color = "darkblue",
            fillOpacity = 0.6,
            group = "selected",
            options = pathOptions(clickable = FALSE)
          )
      } else {
        showNotification("This polygon is already selected", type = "warning", duration = 3)
      }
    } else {
      showNotification("No polygon selected", type = "warning", duration = 3)
    }
  })
  
  # Clear all selections
  observeEvent(input$clear_selection, {
    selected_polygons(character(0))
    selected_polygon_id(NULL)
    
    # Clear highlighting and selection on map
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      clearGroup("selected")
    
    # Show notification
    showNotification("All selections cleared", type = "message", duration = 3)
  })
  
  # Render data table
  output$table <- renderDataTable({
    subset <- table_data()
    if (nrow(subset) == 0) {
      return(data.frame())
    }
    
    # Select columns and convert to dataframe
    df <- subset %>%
      st_drop_geometry() %>%
      select(oid_1:lga)
    
    if ("property_n" %in% colnames(df)) {
      df <- df %>% select(-property_n)
    }
    
    df
  }, options = list(pageLength = 10, searching = TRUE))
  
  # Download handler for selected properties
  output$download_data <- downloadHandler(
    filename = function() {
      paste("selected_properties_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      # Get the current table data
      data <- table_data() %>%
        st_drop_geometry()
      write.csv(
        data[, !(names(data) %in% c('property_n', 'st_area_sh', 'st_perimet', 'object_id'))], 
        file
      )
    }
  )
}

shinyApp(ui, server)