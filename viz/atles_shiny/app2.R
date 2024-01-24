library(shiny)
library(leaflet)
library(shinythemes)
library(tidyr)
library(dplyr)
library(lubridate)
library(shinydashboard)

# UI definition
ui <-
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    navbarPage(
       #theme = shinytheme("flatly"),
       title=div( style = 'padding-left: 15px',
         img(src="sindicat.svg", width = "70px"),
         div("Atles de l'Habitatge", style = '
          display: inline;
          margin-right: 0px;
          margin-left: 14px;
          ')),
       tabPanel(
        "Mapa",
        fluidPage(
          sidebarLayout(
            sidebarPanel(
              textOutput('poligonId'),
              width = 3,
              fluidRow(
                infoBox("New Orders", 10 * 2, icon = icon("credit-card"))
              )
            ),
            mainPanel(
              leafletOutput(outputId = "map", width = "100%", height = "600px")
            ),
            position = "left",
            fluid = TRUE
          )
        )
       )
      )
)
# Server logic
server <- function(input, output, session) {
  load('data/data.RData')
  basemap <- atles_basemap |> mutate(valor=1) |> mutate(id = row_number())

  rv <- reactiveVal()

  pal <- colorNumeric(palette = "RdBu", domain = c(25:50))
  output$map <- renderLeaflet({
    leaflet(data = basemap) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(valor),
        weight = 2,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = '#666',
          dashArray = '',
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~as.character(nom),
        layerId = ~id,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = 'auto'
        )
      )
  })

  observeEvent(input$map_shape_click, {
    rv(input$map_shape_click$id)
  })

  output$poligonId <- renderText({rv()})
}

# Run the application
shinyApp(ui = ui, server = server)
