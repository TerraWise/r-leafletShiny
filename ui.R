library(shiny)
library(bslib)

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