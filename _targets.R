library(targets)
library(tarchetypes)

source('R/idealista_functions.R')
source('R/locations_functions.R')

tar_option_set(packages =
  c('readr','dplyr','tidyr','lubridate','stringr','ggplot2','mapSpain')
)

list(
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
    locations,
    get_locations(barris, municipis, comarques, provincies)
  )
)





