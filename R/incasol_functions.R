


# Lloguers municipis AMB per barris
# https://habitatge.gencat.cat/ca/dades/indicadors_estadistiques/estadistiques_de_construccio_i_mercat_immobiliari/mercat_de_lloguer/lloguers-municipis-amb/
## lloguer trimestral barris AMB €/m2
get_lloguer_trimestral_barris_AMB_m2 <- function() {
  url <- "https://habitatge.gencat.cat/web/.content/home/dades/estadistiques/01_Estadistiques_de_construccio_i_mercat_immobiliari/03_Mercat_de_lloguer/05_Lloguers_AMB/AMB_lloguer_m2.xlsx"
  destfile <- "AMB_lloguer_m2.xlsx"
  curl::curl_download(url, destfile)
  AMB_lloguer_m2 <- read_excel(destfile, skip = 5)
  if (file.exists(destfile)) file.remove(destfile)
  AMB_lloguer_m2
}



# Lloguers Barcelona per districtes i barris
# https://habitatge.gencat.cat/ca/dades/indicadors_estadistiques/estadistiques_de_construccio_i_mercat_immobiliari/mercat_de_lloguer/lloguers-barcelona-per-districtes-i-barris/
## lloguer trimestral barris BCN €/m2
get_incasol_lloguer_trimestral_barris_BCN_m2 <- function() {
  url <- "https://habitatge.gencat.cat/web/.content/home/dades/estadistiques/01_Estadistiques_de_construccio_i_mercat_immobiliari/03_Mercat_de_lloguer/03_Lloguers_Barcelona_per_districtes_i_barris/trimestral_bcn_lloguer_m2.xlsx"
  destfile <- "trimestral_bcn_lloguer_m2.xlsx"
  curl::curl_download(url, destfile)
  sheets <- readxl::excel_sheets(destfile)

  trimestral_bcn_lloguer_m2 = tibble()

  for (i in as.character(2014:2023)) {
    tmp <- read_excel(destfile, skip = 19, sheet = i) |>
      transmute(
        barri_codi = sprintf("%02d", as.numeric(`...1`)),
        barri_nom_incasol = `Barris (1)`,
        municipi_codi = '019',
        provincia_codi = '08',
        t1 = as.numeric(`...3`),
        t2 = as.numeric(`...4`),
        t3 = as.numeric(`...5`),
        t4 = as.numeric(`...6`),
        anual = as.numeric(across(last_col())[[1]]),
      ) |>
      pivot_longer(cols = c(t1, t2, t3, t4, anual),
        names_to = "periode", values_to = "incasol_lloguer_m2") |>
      mutate(
        data_inici = case_when(
          periode == 't1' ~ ymd(paste0(i, '-01-01')),
          periode == 't2' ~ ymd(paste0(i, '-04-01')),
          periode == 't3' ~ ymd(paste0(i, '-07-01')),
          periode == 't4' ~ ymd(paste0(i, '-10-01')),
          periode == 'anual' ~ ymd(paste0(i, '-01-01'))
        ),
        periode = ifelse(periode == 'anual', 'anual', 'trimestral')) |>
      filter(row_number() <= n()-3)
    trimestral_bcn_lloguer_m2 <- bind_rows(trimestral_bcn_lloguer_m2, tmp) |>
      filter(!is.na(barri_codi))
    rm(tmp)
  }
  
  if (file.exists(destfile)) file.remove(destfile)
  
  trimestral_bcn_lloguer_m2 |>
    mutate(
      data_fi = ifelse(
        periode == 'trimestral',
        data_inici + months(3) - days(1),
        data_inici + months(12) - days(1)
      )
    ) |>
    filter(!is.na(incasol_lloguer_m2))
}

get_incasol_lloguer_trimestral_barris_bcn <- function() {
  url <- 'https://habitatge.gencat.cat/web/.content/home/dades/estadistiques/01_Estadistiques_de_construccio_i_mercat_immobiliari/03_Mercat_de_lloguer/03_Lloguers_Barcelona_per_districtes_i_barris/trimestral_bcn_lloguer.xlsx'
  destfile <- "trimestral_bcn_lloguer.xlsx"
  curl::curl_download(url, destfile)
  sheets <- readxl::excel_sheets(destfile)

  trimestral_bcn_lloguer = tibble()

  for (i in as.character(2014:2023)) {
    tmp <- read_excel(destfile, skip = 19, sheet = i) |>
      transmute(
        barri_codi = sprintf("%02d", as.numeric(`...1`)),
        barri_nom_incasol = `Barris (1)`,
        municipi_codi = '019',
        provincia_codi = '08',
        t1 = as.numeric(`...3`),
        t2 = as.numeric(`...4`),
        t3 = as.numeric(`...5`),
        t4 = as.numeric(`...6`),
        anual = as.numeric(across(last_col())[[1]]),
      ) |>
      pivot_longer(cols = c(t1, t2, t3, t4, anual),
                   names_to = "periode", values_to = "incasol_lloguer") |>
      mutate(
        data_inici = case_when(
          periode == 't1' ~ ymd(paste0(i, '-01-01')),
          periode == 't2' ~ ymd(paste0(i, '-04-01')),
          periode == 't3' ~ ymd(paste0(i, '-07-01')),
          periode == 't4' ~ ymd(paste0(i, '-10-01')),
          periode == 'anual' ~ ymd(paste0(i, '-01-01'))
        ),
        periode = ifelse(periode == 'anual', 'anual', 'trimestral')) |>
      filter(row_number() <= n()-3)
    trimestral_bcn_lloguer <- bind_rows(trimestral_bcn_lloguer, tmp) |>
      filter(!is.na(barri_codi))
    rm(tmp)
  }
  
  if (file.exists(destfile)) file.remove(destfile)
  
  trimestral_bcn_lloguer |>
    mutate(
      data_fi = case_when(
        periode == 'trimestral' ~ data_inici + months(3) - days(1),
        TRUE ~ data_inici + months(12) - days(1)
      )
    ) |>
    filter(!is.na(incasol_lloguer))
}

get_incasol_lloguer_trimestral_barris_bcn_newest <- function(df) {
  df |>
    group_by(barri_codi, municipi_codi, provincia_codi) |>
    arrange(desc(data_fi)) |>
    filter(row_number()==1) |>
    ungroup()
}

# Preu mitjà del lloguer d’habitatges per municipi
# https://analisi.transparenciacatalunya.cat/Habitatge/Preu-mitj-del-lloguer-d-habitatges-per-municipi/qww9-bvhh/data_preview
get_incasol_lloguer_trimestral_municipis <- function() {
  url <- "https://analisi.transparenciacatalunya.cat/api/views/qww9-bvhh/rows.csv?accessType=DOWNLOAD"
  destfile <- "lloguer_trimestral_municipis.csv"
  curl::curl_download(url, destfile)
  lloguer_trimestral_municipis <- read_csv(destfile)
  if (file.exists(destfile)) file.remove(destfile)
  lloguer_trimestral_municipis |>
    transmute(
      municipi_codi = stringr::str_extract(`Codi territorial`,'...$'),
      provincia_codi = stringr::str_extract(`Codi territorial`,'..'),
      periode = 'pseudotrimestral',
      data_inici = case_when(
        Període == 'gener-setembre' ~ ymd(paste0(as.character(Any), '-01-01')),
        Període == 'juliol-setembre' ~ ymd(paste0(as.character(Any), '-07-01')),
        Període == 'abril-juny' ~ ymd(paste0(as.character(Any), '-04-01')),
        Període == 'gener-juny' ~ ymd(paste0(as.character(Any), '-01-01')),
        Període == 'gener-març' ~ ymd(paste0(as.character(Any), '-01-01')),
        Període == 'gener-desembre' ~ ymd(paste0(as.character(Any), '-01-01')),
        Període == 'octubre-desembre' ~ ymd(paste0(as.character(Any), '-10-01'))
      ),
      data_fi = case_when(
        Període == 'gener-setembre' ~ ymd(paste0(as.character(Any), '-09-30')),
        Període == 'juliol-setembre' ~ ymd(paste0(as.character(Any), '-09-30')),
        Període == 'abril-juny' ~ ymd(paste0(as.character(Any), '-06-30')),
        Període == 'gener-juny' ~ ymd(paste0(as.character(Any), '-06-30')),
        Període == 'gener-març' ~ ymd(paste0(as.character(Any), '-03-31')),
        Període == 'gener-desembre' ~ ymd(paste0(as.character(Any), '-12-31')),
        Període == 'octubre-desembre' ~ ymd(paste0(as.character(Any), '-12-31'))
      ),
      incasol_lloguer = Renda,
      incasol_nous_llogers = Habitatges
    ) |>
    filter(!is.na(incasol_lloguer))
}


get_incasol_lloguer_trimestral_municipis_newest <- function(df) {
  df |>
    group_by(municipi_codi, provincia_codi) |>
    arrange(desc(data_fi)) |>
    filter(row_number()==1) |>
    ungroup()
}
