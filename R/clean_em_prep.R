#' Join AJUDA site metadata and clean processed monthly enhanced monitoring PrEP dataframe
#' @param df Processed monthly enhanced monitoring PrEP dataframe
#' @return Final PrEP dataframe used in analytic dashboards
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- clean_em_prep()}
#'
clean_em_prep <- function(df){

  prep_tidy_historic <- df %>%
    dplyr::select(-c(No,
                     Partner,
                     Province,
                     District,
                     `Health Facility`,
                     `SISMA Code`,
                     Relatorio_period,
                     Relatorio_Date)) %>%
    dplyr::mutate(across(starts_with('PrEP_'), ~ tidyr::replace_na(., 0))) %>%
    dplyr::left_join(ajuda_site_map, by = c("Datim Code" = "datim_uid")) %>%
    dplyr::rename(datim_uid = `Datim Code`) %>%
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
                  pop_type,
                  disaggregate,
                  sex,
                  age,
                  starts_with("PrEP_"))

}
