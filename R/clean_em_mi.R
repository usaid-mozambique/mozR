#' Join AJUDA site metadata and clean processed monthly enhanced monitoring MQ dataframe
#' @param df Processed monthly enhanced monitoring MQ dataframe
#' @return Final MQ dataframe used in analytic dashboards
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- clean_em_mi()}

clean_em_mi <- function(df){

  df_cleaned <- df %>%

    dplyr::select(!c(partner, snu, psnu, sitename)) %>% # strip meta data that will be replaced by sitemap

    dplyr::left_join(ajuda_site_map, by = "datim_uid") %>%

    dplyr::relocate(sisma_uid:site_nid, .after = datim_uid) %>%

    dplyr::relocate(period:partner, .after = site_nid) %>%

    dplyr::mutate(row_n = row_number()) %>%

    tidyr::pivot_wider(names_from = indicator, values_from = value) %>%

    dplyr::select(!row_n)

  return(df_cleaned)

}
