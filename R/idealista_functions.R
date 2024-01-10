
read_data_catalunya <- function(file) {
  read_csv(
    file,
    col_types = cols(...1 = col_skip(), period = col_date(format = "%Y-%m-%d"))
  ) |>
    filter(province %in%
      c("Barcelona","Girona","Lleida","Tarragona")
    ) |>
    distinct()
}


add_loc <- function(idealista) {
  idealista |>
    transmute(
      location_id,
      level,
      name,
      province,
      town,
      district,
      neighborhood,
      period,
      operation,
      price
    )
}


get_idealista_municipis_prices_loc_wide <- function(idealista, locations) {
  idealista |>
    filter(level == 'town') |>
    transmute(
      municipi_codi = str_sub(location_id, start = 19, end = 22),
      provincia_codi = str_sub(location_id, start = 9, end = 10),
      period,
      stock_ads_rent,
      stock_ads_sale,
      stock_cadastre,
      representative_age,
      representative_size,
      stock_residential_building,
      operation,
      price
    ) |>
    pivot_wider(names_from = operation, values_from = price) |>
    transmute(
      provincia_codi,
      municipi_codi,
      periode = 'mensual',
      data_inici = lubridate::floor_date(period, 'month'),
      #idealista_stock_residential_building = stock_residential_building, # a data de de l'scraping
      #idealista_stock_cadastre = stock_cadastre, # a data de de l'scraping
      #idealista_stock_ads_sale = stock_ads_sale, # a data de de l'scraping
      #idealista_stock_ads_rent = stock_ads_rent, # a data de de l'scraping
      idealista_sale_price = sale,
      idealista_rent_price = rent
    )
}




