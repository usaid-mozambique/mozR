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

  df <- df %>%

    dplyr::select(!c(partner, snu, psnu, sitename)) %>% # strip meta data that will be replaced by sitemap

    dplyr::left_join(ajuda_site_map, by = "datim_uid") %>%

    dplyr::relocate(sisma_uid:site_nid, .after = datim_uid) %>%

    dplyr::relocate(period:partner_pepfar_clinical, .after = site_nid) %>%

    dplyr::mutate(row_n = row_number()) %>%

    tidyr::pivot_wider(names_from = indicator, values_from = value) %>%

    dplyr::select(datim_uid,
                  sisma_uid,
                  sisma_uid_datim_map,
                  site_nid,
                  period,
                  snu,
                  psnu,
                  sitename,
                  partner = partner_pepfar_clinical,
                  numdenom,
                  pop_type,
                  age,
                  program_ap3,
                  program_mi,
                  dpi.colheu.pcr_D:cv.receberam.1e2a ) # to exclude new cd4 and revelation indicators, the final variable would be mds.cv.estaveis

  return(df)

}
