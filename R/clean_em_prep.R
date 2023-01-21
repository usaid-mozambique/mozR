#' Join AJUDA site metadata and clean processed monthly enhanced monitoring PrEP dataframe
#' @param df Processed monthly enhanced monitoring PrEP dataframe
#' @return Final PrEP dataframe used in analytic dashboards
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- clean_em_prep()}

clean_em_prep <- function(df){

  df_cleaned <- df %>%

    dplyr::select(!c(partner, snu, psnu, sitename)) %>% # strip meta data that will be replaced by sitemap

    dplyr::mutate(row_n = row_number()) %>%

    tidyr::pivot_wider(names_from =  indicator, values_from = value) %>%

    dplyr::mutate(across(starts_with('PrEP_'), ~ tidyr::replace_na(., 0))) %>%

    dplyr::left_join(ajuda_site_map, by = "datim_uid") %>%

    dplyr::select(datim_uid,
                  sisma_uid,
                  sisma_uid_datim_map,
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

  return(df_cleaned)

}
