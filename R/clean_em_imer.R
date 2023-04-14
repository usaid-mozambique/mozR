#' Join AJUDA site metadata and clean processed monthly enhanced monitoring IMER dataframe
#' @param df Processed monthly enhanced monitoring IMER dataframe
#' @return Final IMER dataframe used in analytic dashboards
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- clean_em_imer()}

clean_em_imer <- function(df){

  temp <- df %>%
    dplyr::filter(indicator == "TX_CURR") %>%
    dplyr::select(period)

  period_limiter <- max(as.Date(temp$period))

  df <- df %>%
    dplyr::filter(period <= as.Date(period_limiter)) %>%
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
                  grm_sernap,
                  cop_entry,
                  ends_with("tude"),
                  starts_with("program_"),
                  starts_with("his_"),
                  indicator,
                  numdenom,
                  pop_type,
                  key_pop,
                  dispensation,
                  er_status,
                  dsd_eligibility,
                  sex,
                  age,
                  value) %>%
    dplyr::mutate(temp_indicator = indicator,
                  temp_value = value) %>%
    tidyr::pivot_wider(
      names_from = temp_indicator,
      values_from = temp_value
    ) %>%
    dplyr::mutate(
      TX_MMD_D = TX_MMD,
      TX_MMD = dplyr::case_when(dispensation %in% c("3-5", "6+") ~ TX_MMD_D),
      indicator = dplyr::case_when(indicator == "TX_MMD" ~ "TX_MMD_D",
                                   TRUE ~ indicator))

  return(df)

}
