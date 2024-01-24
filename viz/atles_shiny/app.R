## app.R ##
library(shiny)
library(leaflet)
library(shinythemes)
library(tidyr)
library(dplyr)
library(lubridate)
library(shinydashboard)


header <- dashboardHeader(
  title = "Atles de l'Habitatge"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Situació del lloguer", tabName = "landpage", icon = icon("map"))
  )
)

mapa_pincipal <- tabItem(
  tabName = 'landpage',
  fluidRow(
    column(
      # leaflet map
      width = 8,
      leafletOutput(outputId = "map", width = "100%", height = "600px")
    ),
    column(
      # pastilla info
      width = 4,
      valueBox(70, "Salari", icon = icon("percent", lib='font-awesome')),
      valueBox(17.67, "Metre quadrat", icon = icon("euro-sign", lib='font-awesome')),
      valueBox(0, "Protegit", icon = icon("thumbs-up", lib='font-awesome')),
    )
  )
)

body <- dashboardBody(
  tabItems(
    mapa_pincipal
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  dashboardBody(
    tabItems(
      mapa_pincipal
    )
  )
)

server <- function(input, output, session) {

  # atles_newest_values_map <- targets::tar_read('atles_newest_values_map')
  # incasol_lloguer_trimestral_municipis <- targets::tar_read('incasol_lloguer_trimestral_municipis')
  # incasol_lloguer_trimestral_barris_bcn <- targets::tar_read('incasol_lloguer_trimestral_barris_bcn')
  # idealista_municipis_prices_loc_wide <- targets::tar_read('idealista_municipis_prices_loc_wide')
  # idealista_barris_bcn_prices_loc_wide <- targets::tar_read('idealista_barris_bcn_prices_loc_wide')

  load('data/data.RData')

  rv <- reactiveVal()

  pal <- colorNumeric("viridis", NULL)
  output$map <- renderLeaflet({
    leaflet(data = atles_newest_values_map) %>%
      addTiles() %>%
      addPolygons(
        stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.6,
        fillColor = ~pal(incasol_lloguer),
        label = ~paste0(nom, ": ", formatC(incasol_lloguer, big.mark = ",")),
        layerId = ~iden
      )
  })

  observeEvent(input$map_shape_click, {
    rv(input$map_shape_click$id)
  })

  output$poligonId <- renderText({rv()})
}

shinyApp(ui, server)
