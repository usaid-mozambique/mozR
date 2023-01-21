#' Join AJUDA site metadata and clean processed monthly enhanced monitoring TPT dataframe
#' @param df Processed monthly enhanced monitoring TPT dataframe
#' @return Final TPT dataframe used in analytic dashboards
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- clean_em_tpt()}


clean_em_tpt <- function(df){

  volumn_period <- df %>% # create site volume object to later join to tpt dataframe
    dplyr::mutate(date = as.Date(period, format =  "%y/%m/%d")) %>%
    dplyr::select(datim_uid, date, indicator, value) %>%
    dplyr::filter(date == max(date),
                  indicator == "TX_CURR") %>%
    dplyr::mutate(site_volume = dplyr::case_when(
      value < 1000 ~ "Low",
      between(value, 1000, 5000) ~ "Medium",
      value > 5000 ~ "High",
      TRUE ~ "Not Reported")) %>%
    dplyr::select(datim_uid, site_volume)

  df_cleaned <- df %>%
    dplyr::select(!c(partner, snu, psnu, sitename)) %>% # strip meta data that will be replaced by sitemap
    dplyr::left_join(ajuda_site_map, by = "datim_uid") %>%
    dplyr::left_join(volumn_period) %>%
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
                  site_volume,
                  ends_with("tude"),
                  starts_with("program_"),
                  starts_with("his_"),
                  indicator,
                  attribute,
                  value)

  return(df_cleaned)

}
