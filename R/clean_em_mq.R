#' Join AJUDA site metadata and clean processed monthly enhanced monitoring MQ dataframe
#' @param df Processed monthly enhanced monitoring MQ dataframe
#' @return Final MQ dataframe used in analytic dashboards
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- clean_em_mq()}

clean_em_mq <- function(df){

  df_cleaned <- df %>%
    dplyr::select(-c(No,
                     SISMA_code,
                     Partner,
                     Province,
                     District,
                     `Health Facility`)) %>%
    dplyr::left_join(ajuda_site_map, by = c("DATIM_code" = "datim_uid")) %>%
    dplyr::rename(datim_uid = DATIM_code,
                  period = month) %>%
    dplyr::relocate(sisma_uid:site_nid, .after = datim_uid) %>%
    dplyr::relocate(period:partner, .after = site_nid) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = value)

  return(df_cleaned)

}
