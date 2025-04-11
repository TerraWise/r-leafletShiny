library(reticulate)
library(bslib)
library(shiny)
library(leaflet)

use_condaenv('climate-tool')

py_run_string('import geopandas as gpd')

py_run_string(
"CPES = gpd.read_file(os.path.join('input', 'CPES'))

filtered = CPES.loc[CPES['enterprise'].eq('Agricultural')]
filtered['client_id'] = None"
)

ui <- page_sidebar(
  title = "LGA Map Viewer",
  sidebar = sidebar(
    selectizeInput(
      "lga_selection",
      "Select your shire:",
      choices = unique(py$filtered$lga),
      multiple = TRUE
    ),
    textOutput("selected_polygon_info")
  ),
  
  leafletOutput("map", height = 500),
  card(
    card_header("Properties"),
    dataTableOutput("table")
  )
)

server <- function(input, output, session) {
  # Reactive value for selected polygon
  selected_polygon_id <- reactiveVal(NULL)
  
  # Filter data based on LGA selection
  subset_cpes <- reactive({
    if (length(input$lga_selection) == 0) {
      return(py$filtered[0,])  # Empty dataframe with same structure
    }
    py$filtered %>% filter(lga %in% input$lga_selection)
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
    
    subset <- subset_cpes()
    row <- subset %>% filter(oid_1 == selected_id)
    
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
    }
    
    m
  })
  
  # Handle polygon clicks
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    selected_polygon_id(click$id)
    
    # Update the map to highlight the selected polygon
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = subset_cpes() %>% filter(oid_1 == click$id),
        fillColor = "yellow",
        weight = 4,
        opacity = 1,
        color = "orange",
        fillOpacity = 0.7,
        group = "highlight",
        options = pathOptions(clickable = FALSE)
      )
  })
  
  # Render data table
  output$table <- renderDataTable({
    subset <- subset_cpes()
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
}

shinyApp(ui, server)
