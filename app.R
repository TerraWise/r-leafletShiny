library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(bslib)
library(DT)
library(leaflet.extras)

ui <- page_sidebar(
  title = "LGA Map Viewer",
  sidebar = sidebar(
    uiOutput("lga_selection_ui"),
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
  # Try to load data
  CPES <- reactive({st_read("www/CPES")})

  filtered <- reactive({
    req(CPES())
    CPES() %>% filter(enterprise == "Agricultural")
  })

  # Create LGA selector UI dynamically after data is loaded
  output$lga_selection_ui <- renderUI({
    req(filtered())
    selectizeInput(
      "lga_selection",
      "Select your shire:",
      choices = unique(filtered()$lga),
      multiple = TRUE
    )
  })

  # Reactive value for selected polygon
  selected_polygon_id <- reactiveVal(NULL)

  # Reactive value to store manually selected polygons
  selected_polygons <- reactiveVal(character(0))

  # Filter data based on LGA selection
  subset_cpes <- reactive({
    req(filtered(), input$lga_selection)
    if (length(input$lga_selection) == 0) {
      return(filtered()[0, ])  # Empty dataframe with same structure
    }
    filtered() %>% filter(lga %in% input$lga_selection)
  })

  # Get data for the table based on manually selected polygons
  table_data <- reactive({
    req(filtered())
    selected_ids <- selected_polygons()
    if (length(selected_ids) == 0) {
      return(filtered()[0,])
    }
    filtered() %>% filter(oid_1 %in% selected_ids)
  })

  # Calculate map center
  map_center <- reactive({
    subset <- subset_cpes()
    if (is.null(subset) ||
        nrow(subset) == 0 ||
        is.null(input$lga_selection) ||
        length(input$lga_selection) == 0) {
      # Default center if no selection (Center of Australia)
      return(c(-25.2744, 133.7751))
    }
    })

    # Calculate centroid for each LGA and average them
    centroids <- reactive({lapply(input$lga_selection, function(lga_name) {
      poly <- subset %>% filter(lga == lga_name)
      if (nrow(poly) > 0) {
        centroid <- st_centroid(st_union(poly$geometry))
        centroid_coords <- st_coordinates(centroid)
        return(centroid_coords)
      }
      return(NULL)
    })})

    # Display information about selected polygon
    output$selected_polygon_info <- renderText({
    selected_id <- selected_polygon_id()
    if (is.null(selected_id)) {
      return("No polygon selected")
    }
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

}

shinyApp(ui, server)