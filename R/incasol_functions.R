


# Lloguers municipis AMB per barris
# https://habitatge.gencat.cat/ca/dades/indicadors_estadistiques/estadistiques_de_construccio_i_mercat_immobiliari/mercat_de_lloguer/lloguers-municipis-amb/
## lloguer trimestral barris AMB €/m2
get_lloguer_trimestral_barris_AMB_m2 <- function() {
  url <- "https://habitatge.gencat.cat/web/.content/home/dades/estadistiques/01_Estadistiques_de_construccio_i_mercat_immobiliari/03_Mercat_de_lloguer/05_Lloguers_AMB/AMB_lloguer_m2.xlsx"
  destfile <- "AMB_lloguer_m2.xlsx"
  curl::curl_download(url, destfile)
  AMB_lloguer_m2 <- read_excel(destfile, skip = 5)
  AMB_lloguer_m2
}



# Lloguers Barcelona per districtes i barris
# https://habitatge.gencat.cat/ca/dades/indicadors_estadistiques/estadistiques_de_construccio_i_mercat_immobiliari/mercat_de_lloguer/lloguers-barcelona-per-districtes-i-barris/
## lloguer trimestral barris BCN €/m2
get_lloguer_trimestral_barris_BCN_m2 <- function() {
  url <- "https://habitatge.gencat.cat/web/.content/home/dades/estadistiques/01_Estadistiques_de_construccio_i_mercat_immobiliari/03_Mercat_de_lloguer/03_Lloguers_Barcelona_per_districtes_i_barris/trimestral_bcn_lloguer_m2.xlsx"
  destfile <- "trimestral_bcn_lloguer_m2.xlsx"
  curl::curl_download(url, destfile)
  trimestral_bcn_lloguer_m2 <- read_excel(destfile, skip = 5)
  trimestral_bcn_lloguer_m2
}
