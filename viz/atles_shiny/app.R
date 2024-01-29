## app.R ##
library(shiny)
library(leaflet)
library(shinythemes)
library(tidyr)
library(dplyr)
library(lubridate)
library(shinydashboard)
library(plotly)


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
      textOutput('poligonId'),
      plotlyOutput('rent_price_evolution_graph')
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

  generate_rdata <- function() {
    atles_newest_values_map <<- targets::tar_read('atles_newest_values_map')
    incasol_lloguer_trimestral_municipis <<- targets::tar_read('incasol_lloguer_trimestral_municipis')
    incasol_lloguer_trimestral_barris_bcn <<- targets::tar_read('incasol_lloguer_trimestral_barris_bcn')
    idealista_municipis_prices_loc_wide <<- targets::tar_read('idealista_municipis_prices_loc_wide')
    idealista_barris_bcn_prices_loc_wide <<- targets::tar_read('idealista_barris_bcn_prices_loc_wide')
    atles_base_map_munis_i_bcn_barris <<- targets::tar_read('atles_base_map_munis_i_bcn_barris')
  }

  load('data/data.RData')

  # Reactiu treu id del polígon clicat
  rv <- reactiveVal()
  observeEvent(input$map_shape_click, {
    rv(input$map_shape_click$id)
  }, ignoreInit = TRUE)
  output$poligonId <- renderText({rv()})

  pal <- colorNumeric("viridis", NULL)
  output$map <- renderLeaflet({
    leaflet(data = atles_newest_values_map) %>%
      addTiles() %>%
      addPolygons(
        stroke = T, smoothFactor = 0.3, fillOpacity = 0.6,
        weight = 0.4,
        fillColor = ~pal(incasol_lloguer),
        label = ~paste0(nom, ": ", formatC(incasol_lloguer, big.mark = ",")),
        layerId = ~iden
      )
  })

  plot_rent_price_evolution_graph <- function(codi_exemple) {
    print(codi_exemple)
    data <- bind_rows(
      incasol_lloguer_trimestral_barris_bcn,
      incasol_lloguer_trimestral_municipis
    ) |>
      left_join(atles_base_map_munis_i_bcn_barris) |>
      filter(iden == codi_exemple) |>
      group_by(data_fi) |>
      arrange(desc(incasol_lloguer)) |>
      filter(row_number()==1) |>
      ungroup() |>
      arrange(desc(data_fi))

    print(summary(data))

    data_idealista <- bind_rows(
      idealista_barris_bcn_prices_loc_wide,
      idealista_municipis_prices_loc_wide
    ) |>
      left_join(atles_base_map_munis_i_bcn_barris) |>
      filter(iden == codi_exemple)

    mitma_lloguer_mitja_p25_habitatge_collectiu <- atles_newest_values_map$mitma_lloguer_mitja_p25_habitatge_collectiu[atles_newest_values_map$iden == codi_exemple]
    mitma_lloguer_mitja_p75_habitatge_collectiu <- atles_newest_values_map$mitma_lloguer_mitja_p75_habitatge_collectiu[atles_newest_values_map$iden == codi_exemple]

    fig <- plot_ly(
      data, x = ~data_fi, y = ~incasol_lloguer,
      name = 'incasol', type = 'scatter', mode = 'lines',
      line = list(color = 'rgb(205, 12, 24)', width = 4)
    )


    ay <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = "<b>idealista</b> preu €/m2 mitjà")

    hline <- function(y = 0, color = "black") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list(color = color, dash = 'dash')
      )
    }

    fig %>%
      add_trace(data = data_idealista,
                x = ~data_fi, y = ~idealista_rent_price,
                name = 'idealista',
                line = list(color = 'green', width = 4), # , dash = 'dash'
                yaxis = "y2"
      ) %>%
      layout(
        title = "Evolució preu lloguer", yaxis2 = ay,
        xaxis = list(title=""),
        yaxis = list(title="<b>incasol</b> preu lloguer mitjà")
      ) %>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             shapes = list(
               hline(mitma_lloguer_mitja_p25_habitatge_collectiu),
               hline(mitma_lloguer_mitja_p75_habitatge_collectiu)
             )
      )
  }

  observeEvent(
    input$map_shape_click,
    output$rent_price_evolution_graph <- renderPlotly(plot_rent_price_evolution_graph(rv())),
    ignoreInit = TRUE
    )

}

shinyApp(ui, server)
