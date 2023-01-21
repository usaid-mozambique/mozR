#' Join AJUDA site metadata and clean processed monthly enhanced monitoring TXTB dataframe
#' @param df Processed monthly enhanced monitoring TXTB dataframe
#' @return Final TXTB dataframe used in analytic dashboards
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- clean_em_txtb()}

clean_em_txtb <- function(df){

  volumn_period <- txtb_historic %>%
    dplyr::select(datim_uid, period, indicator, value) %>%
    dplyr::filter(period == max(period),
                  indicator == "TX_CURR") %>%
    dplyr::group_by(datim_uid, .drop = TRUE) %>%
    dplyr::summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
    dplyr::mutate(site_volume = dplyr::case_when(
      value < 1000 ~ "Low",
      between(value, 1000, 5000) ~ "Medium",
      value > 5000 ~ "High",
      TRUE ~ "Not Reported")) %>%
    dplyr::select(datim_uid, site_volume)

  df_cleaned <- txtb_historic %>%
    dplyr::select(!c(partner, snu, psnu, sitename)) %>% # strip meta data that will be replaced by sitemap
    dplyr::left_join(ajuda_site_map, by = "datim_uid") %>%
    dplyr::left_join(volumn_period) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = value) %>%
    dplyr::select(datim_uid,
                  sisma_uid,
                  sisma_uid_datim_map,
                  site_nid,
                  period,
                  partner = partner_pepfar_clinical,
                  snu,
                  psnu,
                  sitename,
                  grm_sernap,
                  cop_entry,
                  adv_disease_phase = program_phase_ahd,
                  site_volume,
                  ends_with("tude"),
                  starts_with("support"),
                  starts_with("his"),
                  sex,
                  age,
                  disaggregate,
                  starts_with("TX_"))

  return(df_cleaned)

}
