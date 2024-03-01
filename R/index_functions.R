library(sf)
library(readr)
library(dplyr)


# Sistema Estatal de Referencia del Precio del Alquiler de Vivienda
# Metodologia:
# https://www.mivau.gob.es/recursos_mfom/comodin/recursos/2024-02-23_metodologia_sistema-estatal-alquiler.pdf

# mapa
# https://www.mivau.gob.es/vivienda/alquila-bien-es-tu-derecho/serpavi

# dades MITMA?
# https://cdn.mitma.gob.es/portal-web-drupal/alquiler/2023-02-07_bd_sistema-indices-alquiler-vivienda_2015-2021.xlsx


calcula_valor_P <- function(mantenimiento, ascensor, aparcamiento,
                            construcción, amueblado, planta, certificado, zonas,
                            piscina, servicios, vistas) {
  
  #ponderaciones_vivienda <- read_csv("ponderaciones_vivienda.csv")
  
  ponderaciones_vivienda <- tibble(
    Campo = c(
      "Mantenimiento y conservación", "Mantenimiento y conservación", "Mantenimiento y conservación", "Mantenimiento y conservación",
      "Ascensor", "Ascensor",
      "Aparcamiento", "Aparcamiento",
      "Año de construcción", "Año de construcción", "Año de construcción", "Año de construcción", "Año de construcción",
      "Amueblado", "Amueblado",
      "Planta", "Planta", "Planta", "Planta", "Planta", "Planta", "Planta",
      "Certificado energético", "Certificado energético", "Certificado energético", "Certificado energético", "Certificado energético", "Certificado energético", "Certificado energético",
      "Zonas comunitarias de uso compartido, como jardín o azotea", "Zonas comunitarias de uso compartido, como jardín o azotea",
      "Piscina comunitaria o equipamientos análogos", "Piscina comunitaria o equipamientos análogos",
      "Servicios de conserjería en el edificio", "Servicios de conserjería en el edificio",
      "Vistas especiales", "Vistas especiales"
    ),
    Variables = c(
      "En perfecto estado", "En buen estado", "Actualizaciones necesarias", "En deficiente estado",
      "Sí", "No",
      "Sí", "No",
      "2008-2019", "1979-2007", "1945-1978", "1918-1944", "antes 1917",
      "Sí", "No",
      "Planta baja", "Planta baja con patio", "Primero o segundo", "Tercero o cuarto", "Quinto o sexto", "Séptimo o más altura", "Ático",
      "A", "B", "C", "D", "E", "F", "G",
      "Sí", "No",
      "Sí", "No",
      "Sí", "No",
      "Sí", "No"
    ),
    Pesos = c(
      10.00, 7.50, 5.00, 2.50,
      10.00, 1.00,
      10.00, 1.00,
      10.00, 6.60, 4.90, 3.20, 1.50,
      10.00, 1.00,
      2.00, 10.00, 2.50, 5.00, 7.50, 10.00, 10.00,
      10.00, 8.60, 7.20, 5.80, 4.40, 3.00, 1.60,
      10.00, 1.00,
      10.00, 1.00,
      10.00, 1.00,
      10.00, 1.00
    ),
    Normalización = c(
      3.50, 3.50, 3.50, 3.50,
      1.75, 1.75,
      1.50, 1.50,
      0.75, 0.75, 0.75, 0.75, 0.75,
      0.75, 0.75,
      0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50,
      0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15,
      0.75, 0.75,
      0.75, 0.75,
      0.75, 0.75,
      0.75, 0.75
    )
  )
  
  
  ponderaciones_vivienda_concreta <- ponderaciones_vivienda |>
    filter(
      (Campo == "Mantenimiento y conservación" & Variables == mantenimiento) |
        (Campo == "Ascensor" & Variables == ascensor) |
        (Campo == "Aparcamiento" & Variables == aparcamiento) |
        (Campo == "Año de construcción" & Variables == construccion) |
        (Campo == "Amueblado" & Variables == amueblado) |
        (Campo == "Planta" & Variables == planta) |
        (Campo == "Certificado energético" & Variables == certificado) |
        (Campo == "Zonas comunitarias de uso compartido, como jardín o azotea" & Variables == zonas) |
        (Campo == "Piscina comunitaria o equipamientos análogos" & Variables == piscina) |
        (Campo == "Servicios de conserjería en el edificio" & Variables == servicios) |
        (Campo == "Vistas especiales" & Variables == vistas)
    )
  P <- sum(ponderaciones_vivienda_concreta$Pesos * ponderaciones_vivienda_concreta$Normalización)
  return(P)
}

calcula_index_espanyol <- function(Smed, S, P25, P75, P, delta = 0.274, gamma = 0.686) {
  log100 <- function(x) {
    log(x, base = 100)
  }
  
  ValorInferior <-
    log100(((99 * ((Smed - 30) / 120) * (P75 - 2.58) / 23.776) + 1)) *
    (0.00000913 * (Smed^3 - S^3) + 0.0034 * (S^2 - Smed^2) +
       0.4351 * (Smed - S) + P25) +
    (1 - log100(((99 * ((Smed - 30) / 120) * ((P75 - 2.58) / 23.776)) + 1))) * P25
  
  ValorSuperior <- log100(((99 * ((Smed - 30) / 120) * ((P75 - 2.58) / 23.776)) + 1)) *
    (0.00001359 * (Smed^3 - S^3) + 0.0053 * (S^2 - Smed^2) +
       0.6889 * (Smed - S) + P75) +
    (1 - log100(((99 * ((Smed - 30) / 120) * ((P75 - 2.58) / 23.776)) + 1))) * P75
  
  
  ValorInferiorCorregit <- ValorInferior + (P75 - P25) * delta * (((P - 18.115) / 100.885) - 0.260)
  ValorSuperiorCorregit <- ValorSuperior + (P75 - P25) * gamma * (((P - 18.115) / 100.885) - 0.729)
  
  return(c(ValorInferiorCorregit * S, ValorSuperiorCorregit * S))
}

# read shpfile
seccions_censals <- st_read("data/mapa_seccions_censales_catalunya/seccions_censals_catalunya.shp")


S <- 50 # S: Superficie de la vivienda.
mantenimiento <- "En perfecto estado"
ascensor <- "Sí"
aparcamiento <- "Sí"
construccion <- "2008-2019"
amueblado <- "Sí"
planta <- "Planta baja con patio"
certificado <- "A"
zonas <- "Sí"
piscina <- "Sí"
servicios <- "Sí"
vistas <- "Sí"


P <- calcula_valor_P(
  mantenimiento, ascensor, aparcamiento,
  construccion, amueblado, planta, certificado, zonas,
  piscina, servicios, vistas
)

seccions_censals$Smed <- seccions_censals$Superficie
seccions_censals$S <- S
seccions_censals$P25 <- seccions_censals$Renta_Perc
seccions_censals$P75 <- seccions_censals$Renta_Pe_1

# iterate rows to calculate index as a new column
for (i in 1:nrow(seccions_censals)) {
  seccions_censals$index_espanyol[i] <- calcula_index_espanyol(
    seccions_censals$Smed[i], seccions_censals$S[i],
    seccions_censals$P25[i], seccions_censals$P75[i], P
  )[2]
  
  seccions_censals$index_espanyol_smed[i] <- calcula_index_espanyol(
    seccions_censals$Smed[i], seccions_censals$Smed[i],
    seccions_censals$P25[i], seccions_censals$P75[i], P
  )[2]
}
seccions_censals <- seccions_censals |>
  filter(!index_espanyol == min(index_espanyol)) # treiem els 0 (-324.2436)


idealista_municipis_prices_newest <- targets::tar_read('idealista_municipis_prices_newest')

idealista_municipis_prices_newest <- idealista_municipis_prices_newest |> 
  select(provincia_codi, municipi_codi, idealista_rent_price) |> 
  filter(!is.na(idealista_rent_price)) |> 
  mutate(idealista_rent_50_m2 = round(idealista_rent_price*S))


seccions_censals <- seccions_censals |> 
  left_join(idealista_municipis_prices_newest, 
    by = c("CPRO" = "provincia_codi", 
            "CMUN" = "municipi_codi")) |> 
  mutate(
    iie = (idealista_rent_50_m2 - index_espanyol),
    iip = ((idealista_rent_50_m2 - index_espanyol)/index_espanyol)*100
  )

sf::write_sf(seccions_censals, "data/mapa_seccions_censales_catalunya/seccions_censals_index_espanyol_preus.shp")



incasol_lloguer_trimestral_municipis_newest <- targets::tar_read('incasol_lloguer_trimestral_municipis_newest')
gene_habitatges_tensionats <- targets::tar_read('gene_habitatges_tensionats') |> 
  mutate(
    codi_ine = stringr::str_extract(municipi_codi_idescat, '.....')
  )

municipis <- seccions_censals |>
  as_tibble() |> 
  group_by(CPRO, CMUN, NMUN) |>
  summarise(
    index_espanyol_smed_max = max(index_espanyol_smed, na.rm = TRUE)
  )

incasol_vs_index <- incasol_lloguer_trimestral_municipis_newest |> 
  left_join(municipis, by = c("provincia_codi" = "CPRO", "municipi_codi" = "CMUN")) |> 
  mutate(
    codi_ine = paste0(provincia_codi, municipi_codi),
    dif = incasol_lloguer-index_espanyol_smed_max
  ) |> 
  left_join(gene_habitatges_tensionats, by = c("codi_ine")) 

