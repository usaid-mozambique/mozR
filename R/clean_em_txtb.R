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

  volumn_period <- df %>%
    dplyr::select(datim_uid, period, TX_CURR) %>%
    dplyr::filter(period == max(period)) %>%
    dplyr::group_by(datim_uid, .drop = TRUE) %>%
    dplyr::summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
    dplyr::mutate(site_volume = dplyr::case_when(
      TX_CURR < 1000 ~ "Low",
      between(TX_CURR, 1000, 5000) ~ "Medium",
      TX_CURR > 5000 ~ "High",
      TRUE ~ "Not Reported")) %>%
    dplyr::select(datim_uid, site_volume)


  df <- df %>%
    dplyr::select(!c(partner, snu, psnu, sitename)) %>% # strip meta data that will be replaced by sitemap
    dplyr::left_join(ajuda_site_map, by = "datim_uid") %>%
    dplyr::left_join(volumn_period, by = "datim_uid") %>%
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

  return(df)

}
