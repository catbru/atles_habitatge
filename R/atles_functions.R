

get_atles_base_map_munis_i_bcn_barris <- function(locations_map) {
  locations_map |>
    as_tibble() |>
    filter(
      (nivell == 'municipi')
      | (nivell == 'barri' & municipi_nom == 'Barcelona')
    ) |> 
    sf::st_as_sf()
}


get_atles_newest_values_map <- function(
    atles_base_map_munis_i_bcn_barris,
    mitma_indice_alquiler_vivienda,
    idealista_municipis_stock_loc_wide,
    idealista_barris_bcn_stock_loc_wide,
    idealista_municipis_prices_newest,
    idealista_barris_bcn_prices_newest,
    gene_habitatges_tensionats,
    incasol_lloguer_trimestral_municipis_newest,
    incasol_lloguer_trimestral_barris_bcn_newest
  ) {
  atles_base_map_munis_i_bcn_barris |>
    as_tibble() |>
    left_join(
      mitma_indice_alquiler_vivienda |>
        select(-periode, -data_inici)
    ) |>
    left_join(
      bind_rows(
        idealista_municipis_stock_loc_wide |> select(-periode, -data_inici),
        idealista_barris_bcn_stock_loc_wide |> select(-periode, -data_inici, -barri_codi_idealista)
      )
    ) |>
    left_join(
      bind_rows(
        idealista_municipis_prices_newest |> select(-periode, -data_inici),
        idealista_barris_bcn_prices_newest |> select(-periode, -data_inici, -barri_codi_idealista)
      )
    ) |>
    left_join(
      gene_habitatges_tensionats
    ) |>
    left_join(
      bind_rows(
        incasol_lloguer_trimestral_municipis_newest |> select(-periode, -data_inici, -data_fi),
        incasol_lloguer_trimestral_barris_bcn_newest |> select(-periode, -data_inici, -data_fi, -barri_nom_incasol)
      )
    ) |>
    mutate(esforc_acces_25k_anuals = round(((incasol_lloguer*12)/25000)*100),1) |>
    sf::st_as_sf()
}
