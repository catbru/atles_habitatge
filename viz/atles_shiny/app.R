## app.R ##
library(shiny)
library(leaflet)
library(shinythemes)
library(tidyr)
library(dplyr)
library(lubridate)
library(shinydashboard)
library(plotly)
library(logger)


header <- dashboardHeader(
  title = "Atles de l'Habitatge"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Situació del lloguer", tabName = "landpage", icon = icon("map"))
  )
)

mapa_pincipal <- tabItem(
  tabName = "landpage",
  fluidRow(
    column(
      # leaflet map
      width = 8,
      leafletOutput(outputId = "map", width = "100%", height = "600px")
    ),
    column(
      # pastilla info
      width = 4,
      textOutput("nom_lloc"),
      textOutput("poligonId"),
      valueBox(70, "Salari", icon = icon("percent", lib = "font-awesome")),
      valueBox(17.67, "Metre quadrat", icon = icon("euro-sign", lib = "font-awesome")),
      valueBox(0, "Protegit", icon = icon("thumbs-up", lib = "font-awesome")),
      plotlyOutput("rent_price_evolution_graph")
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
    atles_newest_values_map <<- targets::tar_read("atles_newest_values_map") |>
      mutate(incasol_lloguer = ifelse(incasol_lloguer == 0, NA, incasol_lloguer))
    incasol_lloguer_trimestral_municipis <<- targets::tar_read("incasol_lloguer_trimestral_municipis")
    incasol_lloguer_trimestral_barris_bcn <<- targets::tar_read("incasol_lloguer_trimestral_barris_bcn")
    idealista_municipis_prices_loc_wide <<- targets::tar_read("idealista_municipis_prices_loc_wide")
    idealista_barris_bcn_prices_loc_wide <<- targets::tar_read("idealista_barris_bcn_prices_loc_wide")
    atles_base_map_munis_i_bcn_barris <<- targets::tar_read("atles_base_map_munis_i_bcn_barris")
  }

  load("data/data.RData")

  ## DATA
  incasol_lloguer_trimestral <- bind_rows(
    incasol_lloguer_trimestral_barris_bcn,
    incasol_lloguer_trimestral_municipis
  ) |>
    left_join(atles_base_map_munis_i_bcn_barris) |>
    group_by(data_fi, iden) |>
    arrange(desc(incasol_lloguer)) |>
    filter(row_number() == 1) |>
    ungroup() |>
    arrange(desc(data_fi))

  idealista_prices <- bind_rows(
    idealista_barris_bcn_prices_loc_wide,
    idealista_municipis_prices_loc_wide
  ) |>
    left_join(atles_base_map_munis_i_bcn_barris)

  ## REACTIVITAT
  rv <- reactiveVal()
  observeEvent(input$map_shape_click, {rv(input$map_shape_click$id)})
  output$poligonId <- renderText({rv()})

  ## MAPA
  domain = min(atles_newest_values_map$incasol_lloguer,na.rm = T):max(atles_newest_values_map$incasol_lloguer,na.rm = T)
  pal <- colorQuantile("inferno", domain=domain, n=12)
  output$map <- renderLeaflet({
    leaflet(data = atles_newest_values_map) %>%
      addTiles() %>%
      addPolygons(
        stroke = T, smoothFactor = 0.3, fillOpacity = 0.6,
        weight = 0.4,
        fillColor = ~ pal(incasol_lloguer),
        label = ~ paste0(nom, ": ", formatC(incasol_lloguer, big.mark = ",")),
        layerId = ~iden
      )
  })

  ## RENT PRICE GRAPH
  plot_rent_price_evolution_graph <- function(codi_exemple) {
    log_info(paste("plot_rent_price_evolution_graph", as.character(codi_exemple)))


    data <- incasol_lloguer_trimestral |> filter(iden == codi_exemple)
    data_idealista <- idealista_prices |> filter(iden == codi_exemple)

    mitma_lloguer_mitja_p25_habitatge_collectiu <- atles_newest_values_map$mitma_lloguer_mitja_p25_habitatge_collectiu[atles_newest_values_map$iden == codi_exemple]
    mitma_lloguer_mitja_p75_habitatge_collectiu <- atles_newest_values_map$mitma_lloguer_mitja_p75_habitatge_collectiu[atles_newest_values_map$iden == codi_exemple]

    hline <- function(y = 0, color = "black") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list(color = color, dash = "dash")
      )
    }

    fig <- plot_ly(
      data,
      x = ~data_fi, y = ~incasol_lloguer,
      name = "incasol", type = "scatter", mode = "lines",
      line = list(color = "rgb(205, 12, 24)", width = 4)
    ) %>%
      layout(
        title = "Evolució preu lloguer",
        xaxis = list(title = ""),
        yaxis = list(title = "<b>incasol</b> preu lloguer mitjà")
      ) %>%
      layout(
        plot_bgcolor = "#e5ecf6",
        xaxis = list(
          zerolinecolor = "#ffff",
          zerolinewidth = 2,
          gridcolor = "ffff"
        ),
        yaxis = list(
          zerolinecolor = "#ffff",
          zerolinewidth = 2,
          gridcolor = "ffff"
        ),
        shapes = list(
          hline(mitma_lloguer_mitja_p25_habitatge_collectiu),
          hline(mitma_lloguer_mitja_p75_habitatge_collectiu)
        )
      )


    if (nrow(data_idealista)) {
      ay <- list(
        tickfont = list(color = "black"),
        overlaying = "y",
        side = "right",
        title = "<b>idealista</b> preu €/m2 mitjà"
      )

      fig <- fig %>%
        add_trace(
          data = data_idealista,
          x = ~data_fi, y = ~idealista_rent_price,
          name = "idealista",
          line = list(color = "green", width = 4), # , dash = 'dash'
          yaxis = "y2"
        ) %>%
        layout(
          title = "Evolució preu lloguer", yaxis2 = ay,
          xaxis = list(title = ""),
          yaxis = list(title = "<b>incasol</b> preu lloguer mitjà")
        )
    }
    fig
  }

  observeEvent(
    input$map_shape_click,
    output$rent_price_evolution_graph <- renderPlotly(plot_rent_price_evolution_graph(rv())),
    ignoreInit = TRUE
  )

  ## Indicadors
  observeEvent(
    input$map_shape_click,
    {
      barri_nom <- atles_newest_values_map$barri_nom[atles_newest_values_map$iden == rv()]
      municipi_nom <- atles_newest_values_map$municipi_nom[atles_newest_values_map$iden == rv()]
      provincia_nom <- atles_newest_values_map$provincia_nom[atles_newest_values_map$iden == rv()]
      idealista_stock_cadastre <- atles_newest_values_map$idealista_stock_cadastre[atles_newest_values_map$iden == rv()]
      idealista_rent_price_m2 <- atles_newest_values_map$idealista_rent_price[atles_newest_values_map$iden == rv()]
      idealista_sale_price_m2 <- atles_newest_values_map$idealista_sale_price[atles_newest_values_map$iden == rv()]
      incasol_lloguer_actual <- atles_newest_values_map$incasol_lloguer[atles_newest_values_map$iden == rv()]
      esforc_acces_25k_anuals <- ((incasol_lloguer_actual*12)/25000)*100
      municipi_declarat_tensionat_2023 <- ifelse(
        atles_newest_values_map$municipi_declarat_tensionat_2023[atles_newest_values_map$iden == rv()] == 1,
        'Sí',
        'No'
      )
      output$nom_lloc <- renderText({
        log_info(paste("barri_nom", as.character(barri_nom)))
        log_info(paste("municipi_nom", as.character(municipi_nom)))
        ifelse(
          !is.na(barri_nom),
          paste(barri_nom, municipi_nom, sep = ', '),
          paste(municipi_nom, provincia_nom, sep = ', ')
        )
      })
    },
    ignoreInit = TRUE
  )

}

shinyApp(ui, server)
