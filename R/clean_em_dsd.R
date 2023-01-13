#' Join AJUDA site metadata and clean processed monthly enhanced monitoring DSD dataframe
#' @param df Processed monthly enhanced monitoring DSD dataframe
#' @return Final DSD dataframe used in analytic dashboards
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- clean_em_dsd()}

clean_em_dsd <- function(df){

  df_cleaned <- df %>%
    dplyr::filter(period <= as.Date(month)) %>%
    dplyr::select(-c(partner,
                     snu,
                     psnu,
                     sitename)) %>%
    dplyr::left_join(ajuda_site_map, by = c("datim_uid" = "datim_uid")) %>%
    dplyr::select(datim_uid,
                  sisma_uid,
                  site_nid,
                  period,
                  partner = partner_pepfar_clinical,
                  snu,
                  psnu,
                  sitename,
                  ends_with("tude"),
                  starts_with("program_"),
                  starts_with("his_"),
                  indicator,
                  pop_type,
                  dsd_eligibility,
                  age,
                  value) %>%
    dplyr::mutate(temp_indicator = indicator,
                  temp_value = value) %>%
    tidyr::pivot_wider(
      names_from = temp_indicator,
      values_from = temp_value)

  return(df_cleaned)

}
