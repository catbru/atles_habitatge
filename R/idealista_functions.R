
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

get_idealista_municipis_prices_loc_wide <- function(df) {
  df |>
    filter(level == 'town') |>
    transmute(
      municipi_codi = str_sub(location_id, start = 19, end = 21),
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
      idealista_sale_price = sale,
      idealista_rent_price = rent
    )
}

get_idealista_municipis_stock_loc_wide <- function(df) {
  df |>
    filter(level == 'town') |>
    transmute(
      municipi_codi = str_sub(location_id, start = 19, end = 21),
      provincia_codi = str_sub(location_id, start = 9, end = 10),
      periode = 'puntual',
      data_inici = ymd('2023-11-01'),
      idealista_stock_residential_building = stock_residential_building, # a data de de l'scraping
      idealista_stock_cadastre = stock_cadastre, # a data de de l'scraping
      idealista_stock_ads_sale = stock_ads_sale, # a data de de l'scraping
      idealista_stock_ads_rent = stock_ads_rent # a data de de l'scraping
    ) |>
    distinct()
}

matching_codis_barris_bcn <- tibble::tribble(
  ~barri_nom, ~barri_codi, ~barri_codi_idealista,
  "Baró de Viver","58",NA,
  "Can Baró","34","07-009",
  "Can Peguera","47","08-007",
  "Canyelles","49",NA,
  "Ciutat Meridiana","55","08-001",
  "Diagonal Mar i el Front Marítim del Poblenou","69","10-009",
  "el Baix Guinardó","33","07-006",
  "el Barri Gòtic","02","01-003",
  "el Besòs i el Maresme","70","10-004",
  "el Bon Pastor","59","09-004",
  "el Camp d'en Grassot i Gràcia Nova","32","06-001",
  "el Camp de l'Arpa del Clot","64","10-001",
  "el Carmel","37","07-007",
  "el Clot","65","10-002",
  "el Coll","29","06-004",
  "el Congrés i els Indians","62","09-005",
  "el Fort Pienc","05","02-006",
  "el Guinardó","35","07-005",
  "el Parc i la Llacuna del Poblenou","66","10-005",
  "el Poble Sec","11","03-007",
  "el Poblenou","68","10-008",
  "el Putxet i el Farró","27","05-006",
  "el Raval","01","01-004",
  "el Turó de la Peira","46",NA,
  "Horta","43","07-002",
  "Hostafrancs","15","03-004",
  "l'Antiga Esquerra de l'Eixample","08","02-002",
  "la Barceloneta","03","01-001",
  "la Bordeta","16","03-003",
  "la Clota","42",NA,
  "la Dreta de l'Eixample","07","02-004",
  "la Font d'en Fargues","36","07-008",
  "la Font de la Guatlla","14","03-006",
  "la Guineueta","48","08-004",
  "la Marina de Port","13","03-005",
  "la Marina del Prat Vermell","12",NA,
  "la Maternitat i Sant Ramon","20","04-002",
  "la Nova Esquerra de l'Eixample","09","02-001",
  "la Prosperitat","52","08-006",
  "la Sagrada Família","06","02-005",
  "la Sagrera","61","09-007",
  "la Salut","30","06-003",
  "la Teixonera","38","07-003",
  "la Trinitat Nova","53","08-003",
  "la Trinitat Vella","57","09-001",
  "la Vall d'Hebron","41",NA,
  "la Verneda i la Pau","73","10-003",
  "la Vila de Gràcia","31","06-002",
  "la Vila Olímpica del Poblenou","67","10-006",
  "les Corts","19","04-001",
  "les Roquetes","50","08-002",
  "les Tres Torres","24","05-004",
  "Montbau","40",NA,
  "Navas","63","09-006",
  "Pedralbes","21","04-003",
  "Porta","45","08-008",
  "Provençals del Poblenou","71","10-007",
  "Sant Andreu","60","09-002",
  "Sant Antoni","10","02-003",
  "Sant Genís dels Agudells","39","07-001",
  "Sant Gervasi - Galvany","26","05-005",
  "Sant Gervasi - la Bonanova","25","05-003",
  "Sant Martí de Provençals","72","10-010",
  "Sant Pere, Santa Caterina i la",NA,NA
)


get_idealista_barris_bcn_prices_loc_wide <- function(df) {
  df |>
    filter(level == 'neighborhood') |>
    filter(town == 'Barcelona') |> 
    transmute(
      municipi_codi = str_sub(location_id, start = 19, end = 21),
      provincia_codi = str_sub(location_id, start = 9, end = 10),
      barri_codi_idealista = str_sub(location_id,  start = 23, end = 28),
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
      barri_codi_idealista,
      periode = 'mensual',
      data_inici = lubridate::floor_date(period, 'month'),
      data_fi = data_inici + months(1) - days(1),
      idealista_sale_price = sale,
      idealista_rent_price = rent
    ) |> 
    left_join(matching_codis_barris_bcn |> select(-barri_nom))
}

get_idealista_barris_bcn_prices_newest_row <- function(df) {
  df |> 
    arrange(desc(data_fi)) |> 
    group_by(provincia_codi, municipi_codi, barri_codi) |> 
    slice(1)
}

get_idealista_barris_bcn_stock_loc_wide <- function(df) {
  df |>
    filter(level == 'neighborhood') |>
    filter(town == 'Barcelona') |> 
    transmute(
      municipi_codi = str_sub(location_id, start = 19, end = 21),
      provincia_codi = str_sub(location_id, start = 9, end = 10),
      barri_codi_idealista = str_sub(location_id,  start = 23, end = 28),
      periode = 'puntual',
      data_inici = ymd('2023-11-01'),
      idealista_stock_residential_building = stock_residential_building, # a data de de l'scraping
      idealista_stock_cadastre = stock_cadastre, # a data de de l'scraping
      idealista_stock_ads_sale = stock_ads_sale, # a data de de l'scraping
      idealista_stock_ads_rent = stock_ads_rent # a data de de l'scraping
    ) |>
    distinct() |> 
    left_join(matching_codis_barris_bcn |> select(-barri_nom))
}
