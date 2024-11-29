## app.R ##
library(shiny)
library(leaflet)
library(tidyr)
library(dplyr)
library(lubridate)
library(shinydashboard)
library(plotly)
library(logger)
library(sf)
library(fresh) # custom theme
library(shinydashboardPlus)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "black"
  ),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440",
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9",
    info_box_bg = "#D8DEE9"
  )
)


header <- dashboardHeader(
  title = "SLL"
) |>
  tagAppendChild(
    div(
      "Atles de l'habitatge",
      style = "
      display: block;
      font-size: 1.5em;
      margin-block-start: 0.5em;
      color: white;
      font-weight: bold;
      margin-right: 50%",
      align = "right"
    ),
    .cssSelector = "nav"
  )

sidebar <- dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(
    menuItem("Situació del lloguer", tabName = "landpage", icon = icon("map"))
  )
)

mapa_pincipal <- tabItem(
  tabName = "landpage",
  fluidRow(
    column(
      # leaflet map
      width = 6,
      tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
      selectInput("nivellMapa", "Tipus de mapa:",
                  c("Municipis" = "municipi",
                    "Barris" = "barri")),
      leafletOutput("map", width = "100%", height = '655px'),
    ),
    column(
      # pastilla info
      width = 6,
      fluidRow(
      titlePanel(
        h1(textOutput("nom_lloc"), align = "center")
      )),
      fluidRow(
        valueBoxOutput("esforc_acces_25k_anuals_box"),
        valueBoxOutput('idealista_rent_price_m2_box'),
        valueBoxOutput('incasol_lloguer_actual_box')
      ),
      fluidRow(
        h3(textOutput("instruccions_inicials"), align = "center"),
        selectInput("metricaevol", "Mètrica:",
                    c("Incasol Lloguer mitjà" = "incasol_lloguer",
                      "Idealista lloguer m2" = "idealista_rent")),
        plotlyOutput("rent_price_evolution_graph")
      )
    )
  )
)


ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = dashboardBody(
    #use_theme(mytheme),
    includeCSS(path = 'www/custom.css'),
    tabItems(
      mapa_pincipal
    )
  ),
  footer = dashboardFooter(
    left = tags$a(href='https://t.me/sindicatlloguer',
                  'Uneix-te al nostre canal de Telegram', class = 'text-footer'),
    right = tags$a(href='https://sindicatdellogateres.org/',
                   tags$img(src='sindicat.svg',width='50'))
  )
)

server <- function(input, output, session) {
  generate_rdata <- function() {
    atles_newest_values_map <<- targets::tar_read("atles_newest_values_map")
    incasol_lloguer_trimestral_municipis <<- targets::tar_read("incasol_lloguer_trimestral_municipis")
    incasol_lloguer_trimestral_barris_bcn <<- targets::tar_read("incasol_lloguer_trimestral_barris_bcn")
    idealista_municipis_prices_loc_wide <<- targets::tar_read("idealista_municipis_prices_loc_wide")
    idealista_barris_bcn_prices_loc_wide <<- targets::tar_read("idealista_barris_bcn_prices_loc_wide")
    atles_base_map_munis_i_bcn_barris <<- targets::tar_read("atles_base_map_munis_i_bcn_barris")
    save(
      atles_newest_values_map,
      incasol_lloguer_trimestral_municipis,
      incasol_lloguer_trimestral_barris_bcn,
      idealista_municipis_prices_loc_wide,
      idealista_barris_bcn_prices_loc_wide,
      atles_base_map_munis_i_bcn_barris,
      file = "viz/atles_shiny/data/data.RData"
    )
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

  ## Instruccions inicials
  output$instruccions_inicials <- renderText(
    "Selecciona una unitat territorial per visualitzar-ne les dades disponibles"
  )

  ## REACTIVITAT
  rv <- reactiveVal()
  observeEvent(input$map_shape_click, {rv(input$map_shape_click$id)})
  output$poligonId <- renderText({rv()})

  ## MAPA
  domain <- min(atles_newest_values_map$esforc_acces_25k_anuals,na.rm = T):max(atles_newest_values_map$esforc_acces_25k_anuals,na.rm = T)
  pal <- colorBin("magma", domain=domain, pretty = T,na.color = '#FF000000')
  output$map <- renderLeaflet({
    leaflet(
        data = atles_newest_values_map |> filter(nivell == input$nivellMapa),
        options = leafletOptions(zoomControl = FALSE)
      ) %>%
      addTiles() %>%
      addPolygons(
        stroke = T, smoothFactor = 0.3, fillOpacity = 0.6,
        weight = 0.4,
        color = "black", opacity = 1,
        fillColor = ~ pal(esforc_acces_25k_anuals),
        label = ~ paste0(nom, ": ", formatC(esforc_acces_25k_anuals, big.mark = ",")),
        layerId = ~iden
      )
  })

  ## RENT PRICE GRAPH
  plot_rent_price_evolution_graph <- function(codi_exemple, nivell, metrica) {
    log_info(paste("plot_rent_price_evolution_graph", as.character(codi_exemple), metrica))

    if (metrica == "incasol_lloguer") {
      
      data_incasol <- incasol_lloguer_trimestral |> filter(iden == codi_exemple & nivell == nivell)
  
      fig.incasol <- plot_ly(
        data_incasol,
        x = ~data_fi, y = ~incasol_lloguer,
        name = "incasol", type = "scatter", mode = "lines",
        line = list(color = "rgb(205, 12, 24)", width = 4)
      ) %>%
        layout(
          title = "Lloguer mitjà segons Incasol",
          xaxis = list(title = ""),
          yaxis = list(title = "<b>incasol</b> preu lloguer mitjà")
        ) %>%
        layout(
          plot_bgcolor = "#ecf0f5",
          paper_bgcolor = "#ecf0f5",
          xaxis = list(
            zerolinecolor = "#ffff",
            zerolinewidth = 2,
            gridcolor = "ffff"
          ),
          yaxis = list(
            zerolinecolor = "#ffff",
            zerolinewidth = 2,
            gridcolor = "ffff"
          )
        )
      return(fig.incasol)
      
    } else if (metrica == "idealista_rent") {
      
      data_idealista <- idealista_prices |> filter(iden == codi_exemple & nivell == nivell)
    
      fig.idealista <- plot_ly(
        data_idealista,
        x = ~data_fi, y = ~idealista_rent_price,
        name = "idealista preu lloguer", type = "scatter", mode = "lines",
        line = list(color = "rgb(205, 12, 24)", width = 4)
      ) %>%
        layout(
          title = "Preu m2 segons Idealista",
          xaxis = list(title = ""),
          yaxis = list(title = "<b>idealista</b> preu €/m2 mitjà")
        ) %>%
        layout(
          plot_bgcolor = "#ecf0f5",
          paper_bgcolor = "#ecf0f5",
          xaxis = list(
            zerolinecolor = "#ffff",
            zerolinewidth = 2,
            gridcolor = "ffff"
          ),
          yaxis = list(
            zerolinecolor = "#ffff",
            zerolinewidth = 2,
            gridcolor = "ffff"
          )
        )
      return(fig.idealista)
      
    } else {
      stop("metrica not found")
    }
  }

  ## Indicadors
  observeEvent(
    input$map_shape_click,
    {
      output$rent_price_evolution_graph <- renderPlotly(plot_rent_price_evolution_graph(rv(), input$nivellMapa, input$metricaevol))
      output$instruccions_inicials <- renderText('')
      barri_nom <- atles_newest_values_map$barri_nom[atles_newest_values_map$iden == rv() & atles_newest_values_map$nivell == input$nivellMapa]
      municipi_nom <- atles_newest_values_map$municipi_nom[atles_newest_values_map$iden == rv() & atles_newest_values_map$nivell == input$nivellMapa]
      provincia_nom <- atles_newest_values_map$provincia_nom[atles_newest_values_map$iden == rv() & atles_newest_values_map$nivell == input$nivellMapa]
      idealista_stock_cadastre <- atles_newest_values_map$idealista_stock_cadastre[atles_newest_values_map$iden == rv() & atles_newest_values_map$nivell == input$nivellMapa]
      idealista_rent_price_m2 <- atles_newest_values_map$idealista_rent_price[atles_newest_values_map$iden == rv() & atles_newest_values_map$nivell == input$nivellMapa]
      idealista_sale_price_m2 <- atles_newest_values_map$idealista_sale_price[atles_newest_values_map$iden == rv() & atles_newest_values_map$nivell == input$nivellMapa]
      incasol_lloguer_actual <- atles_newest_values_map$incasol_lloguer[atles_newest_values_map$iden == rv() & atles_newest_values_map$nivell == input$nivellMapa]
      esforc_acces_25k_anuals <- atles_newest_values_map$esforc_acces_25k_anuals[atles_newest_values_map$iden == rv() & atles_newest_values_map$nivell == input$nivellMapa]
      
      output$esforc_acces_25k_anuals_box <- renderValueBox({
        valueBox(
          paste(esforc_acces_25k_anuals, "%"), "d'un salari de 25k anual", icon = icon("briefcase", lib='font-awesome'),
          color = "red"
        )
      })

      output$idealista_rent_price_m2_box <- renderValueBox({
        valueBox(
          paste(
            ifelse(is.na(idealista_rent_price_m2), '', idealista_rent_price_m2),
            "€/m2"), "Mitjana anuncis d'Idealista", icon = icon("building", lib='font-awesome'),
          color = "red"
        )
      })

      output$incasol_lloguer_actual_box <- renderValueBox({
        valueBox(
          paste(round(incasol_lloguer_actual,2), "€"), "Mitjana lloguer segons Incasol", icon = icon("tag", lib='font-awesome'),
          color = "red"
        )
      })


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
