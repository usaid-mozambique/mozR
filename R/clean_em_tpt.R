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
  volumn_period <- df %>%
    dplyr::mutate(date = as.Date(Period, format =  "%y/%m/%d")) %>%
    dplyr::select(DATIM_code, date, indicator, value) %>%
    dplyr::filter(date == max(date),
                  indicator == "TX_CURR") %>%
    dplyr::mutate(site_volume = dplyr::case_when(
      value < 1000 ~ "Low",
      between(value, 1000, 5000) ~ "Medium",
      value > 5000 ~ "High",
      TRUE ~ "Not Reported")) %>%
    dplyr::select(DATIM_code, site_volume)

  df_cleaned <- df %>%
    dplyr::left_join(ajuda_site_map, by = c("DATIM_code" = "datim_uid")) %>%
    dplyr::left_join(volumn_period) %>%
    dplyr::select(datim_uid = DATIM_code,
                  sisma_uid,
                  site_nid,
                  period = Period,
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
