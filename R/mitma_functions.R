

# Índice alquiler de vivienda
# https://cdn.mitma.gob.es/portal-web-drupal/alquiler/2023-02-07_bd_sistema-indices-alquiler-vivienda_2015-2021.xlsx
get_indice_alquiler_vivienda <- function() {
  url <- "https://cdn.mitma.gob.es/portal-web-drupal/alquiler/2023-02-07_bd_sistema-indices-alquiler-vivienda_2015-2021.xlsx"
  destfile <- "2023-02-07_bd_sistema-indices-alquiler-vivienda_2015-2021.xlsx"
  curl::curl_download(url, destfile)
  indice_alquiler_vivienda <- read_excel(destfile, skip = 0,sheet = 'Municipios')
  indice_alquiler_vivienda |> 
    transmute(
      municipi_codi = CUMUN_A,
      provincia_codi = CPRO_A,
      periode = 'anual',
      data_inici = ymd('2021-01-01'),
      mitma_lloguer_m2_habitatge_collectiu = ALQM2mes_LV_M_VC_21,
      mitma_lloguer_m2_p25_habitatge_collectiu = ALQM2mes_LV_25_VC_21,
      mitma_lloguer_m2_p75_habitatge_collectiu = ALQM2mes_LV_75_VC_21,
      mitma_lloguer_m2_habitatge_unifamiliar = ALQM2mes_LV_M_VU_21,
      mitma_lloguer_m2_p25_habitatge_unifamiliar = ALQM2mes_LV_25_VU_21,
      mitma_lloguer_m2_p75_habitatge_unifamiliar = ALQM2mes_LV_75_VU_21,
      mitma_lloguer_mitja_habitatge_collectiu = ALQTBID12_M_VC_21,
      mitma_lloguer_mitja_p25_habitatge_collectiu = ALQTBID12_25_VC_21,
      mitma_lloguer_mitja_p75_habitatge_collectiu = ALQTBID12_75_VC_21,
      mitma_lloguer_mitja_habitatge_unifamiliar = ALQTBID12_M_VU_21,
      mitma_lloguer_mitja_p25_habitatge_unifamiliar = ALQTBID12_25_VU_21,
      mitma_lloguer_mitja_p75_habitatge_unifamiliar = ALQTBID12_75_VU_21,
    )
}

# aqui està en pdf no imatge:
# https://govern.cat/govern/docs/2023/08/16/12/59/be087251-45ab-4a89-9600-df22e51e16a0.pdf
get_habitatges_tensionats <- function() {
  read_delim(
    "data/mitma_generalitat_municipis_tensionats.csv", 
     delim = "\t", escape_double = FALSE, 
     locale = locale(encoding = "ISO-8859-1"), 
     trim_ws = TRUE) |> 
  transmute(
    municipi_codi_idescat = Codi,
    municipi_declarat_tensionat_2023 = 1
  )
}