library(sf)         # Simple features for R
library(tmap)       # Thematic Maps
library(tmaptools)  # Thematic Maps Tools

tmap_mode("view")

targets::tar_read(locations_map) |>
  filter(municipi_ambit_funcional == 'Metropolità' & nivell == 'barri') |>
  mutate(valor=1) |>
  tm_shape() +
    tm_basemap(server = "OpenStreetMap") +
    tm_fill("valor", palette = "viridis",alpha = 0.4) +
    tm_layout(legend.outside = TRUE, frame = FALSE) +
    tm_text("nom", size = 0.3,  auto.placement = F, legend.size.show = FALSE)  +
    tm_scale_bar(position = c("RIGHT", "BOTTOM"))



targets::tar_read(locations_map) |>
  filter(nivell == 'barri') |>
  mutate(valor=1) |>
  tm_shape() +
  tm_basemap(server = "OpenStreetMap") +
  tm_fill("valor", palette = "viridis",alpha = 0.4) +
  tm_layout(legend.outside = TRUE, frame = FALSE) +
  tm_text("nom", size = 0.6,  auto.placement = F, legend.size.show = FALSE)  +
  tm_scale_bar(position = c("RIGHT", "BOTTOM"))


targets::tar_read(atles_base_map_munis_i_bcn_barris) |>
  mutate(valor=1) |>
  tm_shape() +
  tm_basemap(server = "OpenStreetMap") +
  tm_fill("valor", palette = "viridis",alpha = 0.4) +
  tm_layout(legend.outside = TRUE, frame = FALSE) +
  tm_text("nom", size = 0.6,  auto.placement = F, legend.size.show = FALSE)  +
  tm_scale_bar(position = c("RIGHT", "BOTTOM"))



library(shiny)
library(leaflet)

# UI definition
ui <- fluidPage(
  titlePanel("Mapa dels Barris amb Leaflet"),
  mainPanel(
    leafletOutput(outputId = "map")
  )
)

# Server logic
server <- function(input, output, session) {

  # Suposem que tens un objecte 'locations_map' disponible aquí que conté les dades espacials
  pal <- colorNumeric(palette = "RdBu", domain = c(25:50))

  output$map <- renderLeaflet({
    leaflet(data = targets::tar_read(locations_map) |>
            filter(nivell == 'barri') |>
              mutate(valor=1)) %>%
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
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = 'auto'
        )
      ) %>%
      addLegend(pal = pal, values = ~valor, opacity = 0.7, title = NULL, position = "bottomright")
  })

  # Observem els clics al mapa
  observeEvent(input$map_shape_click, {
    # Accedim a la informació de l'element clicat
    clicked_feature <- input$map_shape_click
    # Fem print a la consola
    print(clicked_feature)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

