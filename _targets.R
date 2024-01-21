library(targets)
library(tarchetypes)

source('R/idealista_functions.R')
source('R/locations_functions.R')
source('R/incasol_functions.R')
source('R/mitma_functions.R')

libraries <- c('readr','dplyr','tidyr','lubridate',
  'stringr','ggplot2','mapSpain','readxl')

tar_option_set(packages = libraries)

pipe_idealista <- list(
  tar_target(
    idealista_prices_file,
    "data/20231229_locations_with_price_evolution.csv",
    format = "file"
  ),
  tar_target(
    idealista_preus_catalunya,
    read_data_catalunya(idealista_prices_file)
  ),
  tar_target(
    idealista_municipis_prices_loc_wide,
    get_idealista_municipis_prices_loc_wide(idealista_preus_catalunya)
  ),
  tar_target(
    idealista_municipis_stock_loc_wide,
    get_idealista_municipis_stock_loc_wide(idealista_preus_catalunya)
  ),
  tar_target(
    idealista_barris_bcn_prices_loc_wide,
    get_idealista_barris_bcn_prices_loc_wide(idealista_preus_catalunya)
  ),
  tar_target(
    idealista_barris_bcn_stock_loc_wide,
    get_idealista_barris_bcn_stock_loc_wide(idealista_preus_catalunya)
  )
)


pipe_map <- list(
  tar_target(
    municipis_mapSpain,
    get_municipis_mapSpain()
  ),
  tar_target(
    codis_comarca_2015,
    get_codis_comarca_2015()
  ),
  tar_target(
    municipis_idescat,
    get_municipis_idescat()
  ),
  tar_target(
    municipis,
    build_municipis(
      municipis_mapSpain,
      municipis_idescat,
      codis_comarca_2015
    )
  ),
  tar_target(
    comarques,
    get_comarques()
  ),
  tar_target(
    provincies,
    get_provincies()
  ),
  tar_target(
    barris_AMB,
    get_barris_AMB()
  ),
  tar_target(
    barris_girona,
    get_barris_girona()
  ),
  tar_target(
    barris,
    get_barris(barris_AMB, barris_girona)
  ),
  tar_target(
    locations_map,
    get_locations(barris, municipis, comarques, provincies)
  ),
  tar_target(
    locations,
    get_locations_as_tibble(locations_map)
  )
)

c(
  pipe_idealista,
  pipe_map,
  list(
    tar_target(
      incasol_lloguer_trimestral_barris_BCN_m2,
      get_incasol_lloguer_trimestral_barris_BCN_m2()
    ),
    tar_target(
      incasol_lloguer_trimestral_municipis,
      get_incasol_lloguer_trimestral_municipis()
    ),
    tar_target(
      mitma_indice_alquiler_vivienda,
      get_indice_alquiler_vivienda()
    ),
    tar_target(
      gene_habitatges_tensionats,
      get_habitatges_tensionats()
    )
  )
)



