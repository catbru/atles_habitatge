
get_codis_comarca_2015 <- function() {
  url <- 'https://analisi.transparenciacatalunya.cat/api/views/txkt-yjtq/rows.csv?fourfour=txkt-yjtq&cacheBust=1703754337&date=20240110&accessType=DOWNLOAD&sorting=true'
  read_csv(
    url,
    col_types = cols(`Georeferència Municipi` = col_skip())
  ) |>
    select(
      municipi_codi_ine = `Codi INE`,
      municipi_comarca_codi_2015 = `Codi Comarca 2015`,
      municipi_ambit_funcional = `Àmbit funcional`
    ) |>
    distinct()
}

get_municipis_idescat <- function() {
  read_csv(
    'https://www.idescat.cat/codis/?id=50&n=9&f=csv',
    col_types = cols(...5 = col_skip()),
    skip = 3) |>
  mutate(
    municipi_codi_ine = stringr::str_extract(Codi, '.....')
  ) |>
  transmute(
    municipi_codi_ine = municipi_codi_ine,
    municipi_codi_idescat = Codi,
    comarca_codi = `Codi comarca`
  )
}

get_municipis_mapSpain <- function() {
  mapSpain::esp_munic.sf |>
    dplyr::filter(ine.ccaa.name %in% c('Cataluña'))
}

build_municipis <- function(municipis_mapSpain, municipis_idescat, codis_comarca_2015) {
  municipis_mapSpain |>
    transmute(
      municipi_codi_ine = LAU_CODE,
      municipi_nom = name,
      municipi_codi = cmun,
      provincia_codi = cpro,
      ccaa_codi = codauto,
      geometry = geometry
    ) |>
    left_join(
      municipis_idescat,
      by = c('municipi_codi_ine' = 'municipi_codi_ine')
    ) |>
    left_join(
      codis_comarca_2015,
      by = 'municipi_codi_ine'
    ) |>
    select(
      municipi_codi_ine,
      municipi_codi_idescat,
      municipi_nom,
      municipi_codi,
      municipi_ambit_funcional,
      municipi_comarca_codi_2015,
      comarca_codi,
      provincia_codi,
      geometry
    )
}

get_comarques <- function() {
  file <-
    'data/locations/comarques/divisions-administratives-v2r1-comarques-1000000-20230928.shp'
  sf::read_sf(file) |>
    transmute(
      comarca_codi = CODICOMAR,
      comarca_nom = NOMCOMAR,
      geometry = geometry
    ) |>
    sf::st_transform(4258)
}

get_provincies <- function() {
  #mapSpain::esp_get_prov_siane(c(
  mapSpain::esp_get_prov(c(
    c("Barcelona","Girona","Lleida","Tarragona")
  )) |>
    transmute(
      provincia_codi = cpro,
      provincia_nom = ine.prov.name,
      provincia_nom_es = iso2.prov.name.es,
      provincia_codi_nuts = nuts.prov.code,
      provincia_codi_iso2 = iso2.prov.code,
      ccaa_codi = codauto,
      ccaa_codi_nuts2 = nuts2.code,
      geometry = geometry
    ) |>
    sf::st_transform(4258)
}

get_barris_AMB <- function() {
  file <-
    'data/locations/barris/AMB/Bc_barris2016_AMB.shp'
  sf::read_sf(file) |>
    transmute(
      barri_nom = Nom,
      barri_codi = str_extract(CodMunBa16,'..$'),
      municipi_codi = str_extract(CodMunBa16,'...'),
      provincia_codi = '08',
      geometry = geometry
    ) |>
    sf::st_transform(4258) |>
    filter(!is.na(barri_codi))
}

get_barris_girona <- function() {
  file <-
    'data/locations/barris/Girona/Barris.shp'
  sf::read_sf(file) |>
    transmute(
      barri_nom = BARRIS,
      barri_codi = as.character(OBJECTID),
      municipi_codi = '079',
      provincia_codi = '17',
      geometry = geometry
    ) |>
    sf::st_transform(4258)
}

get_barris <- function(barris_AMB, barris_girona) {
  dplyr::bind_rows(barris_AMB, barris_girona)
}

get_locations <- function(barris, municipis, comarques, provincies) {
  locations_barris <- barris |>
    mutate(
      nom = barri_nom,
      nivell = 'barri',
      .before = barri_nom) |>
    left_join(municipis |> as_tibble() |> select(-geometry)) |>
    left_join(comarques |> as_tibble() |> select(-geometry)) |>
    left_join(provincies |> as_tibble() |> select(-geometry)) |>
    mutate(iden = paste0('BARR',barri_codi, municipi_codi)) |>
    dplyr::relocate(geometry, .after = last_col())

  locations_munis <- municipis |>
    mutate(
      iden = paste0('MUNI',municipi_codi_ine),
      nom = municipi_nom,
      nivell = 'municipi',
      barri_nom = NA,
      barri_codi = NA,
      .before = municipi_codi_ine) |>
    left_join(comarques |> as_tibble() |> select(-geometry)) |>
    left_join(provincies |> as_tibble() |> select(-geometry)) |>
    dplyr::relocate(geometry, .after = last_col())

  locations_comarques <- comarques |>
    mutate(
      iden = paste0('COMA',comarca_codi),
      nom = comarca_nom,
      nivell = 'comarca',
      barri_nom = NA,
      barri_codi = NA,
      municipi_codi_ine = NA,
      municipi_codi_idescat = NA,
      municipi_nom = NA,
      municipi_codi = NA,
      municipi_ambit_funcional = NA,
      municipi_comarca_codi_2015 = NA,
      .before = comarca_codi) |>
    left_join(municipis |> as_tibble() |> select(comarca_codi, provincia_codi) |> distinct()) |>
    left_join(provincies |> as_tibble() |> select(-geometry)) |>
    dplyr::relocate(geometry, .after = last_col())

  locations_provincies <- provincies |>
    mutate(
      iden = paste0('PROV',provincia_codi),
      nom = provincia_nom,
      nivell = 'provincia',
      barri_nom = NA,
      barri_codi = NA,
      municipi_codi_ine = NA,
      municipi_codi_idescat = NA,
      municipi_nom = NA,
      municipi_codi = NA,
      municipi_ambit_funcional = NA,
      municipi_comarca_codi_2015 = NA,
      comarca_codi = NA,
      comarca_nom = NA,
      .before = municipi_codi_ine) |>
    left_join(provincies |> as_tibble() |> select(-geometry)) |>
    dplyr::relocate(geometry, .after = last_col())

  bind_rows(locations_barris, locations_munis, locations_comarques, locations_provincies)
}

get_locations_as_tibble <- function(locations_map) {
  locations_map |> as_tibble() |> select(-geometry)
}
