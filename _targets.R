library(targets)
library(tarchetypes)

source('R/idealista_functions.R')

tar_option_set(packages =
  c('readr','dplyr','tidyr','lubridate','stringr','ggplot2','mapSpain','quarto')
)

list(
  tar_target(
    idealista_prices_file,
    "data/20231229_idealista_price_evolution_ppcc.csv",
    format = "file"
  ),
  tar_target(
    idealista_preus_catalunya,
    read_data_catalunya(idealista_prices_file)
  )
)






