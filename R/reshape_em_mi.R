#' Process monthly enhanced monitoring MQ submission from PEPFAR Mozambique Clinical Partners
#' @param filename Local path to the monthly IP submission
#' @param ip IP whose submission the file pertains to
#' @return A tidy dataframe with monthly enhanced monitoring MQ results
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- reshape_em_mi()}

reshape_em_mi <- function(filename, ip){

  df_cleaned <- readxl::read_excel(filename,
                           sheet = "Monitoria Intensiva",
                           skip = 9,
                           col_types = c("text",
                                         "text", "text", "text", "text", "text",
                                         "text", "text", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric")) %>%

    dplyr::select(-c(No, Data, SISMA_code)) %>%

    dplyr::rename(partner = Partner,
                  snu = Province,
                  psnu = District,
                  sitename = `Health Facility`,
                  datim_uid = DATIM_code,
                  dpi.colheu.pcr_d__all = dpi.colheu.pcr_d_total,
                  dpi.colheu.pcr_n__all = dpi.colheu.pcr_n_total,
                  dpi.pcr.enviado_d__all = dpi.pcr.enviado_d_total,
                  dpi.pcr.enviado_n__all = dpi.pcr.enviado_n_total,
                  dpi.pcr.entregue_d__all = dpi.pcr.entregue_d_total,
                  dpi.pcr.entregue_n__all = dpi.pcr.entregue_n_total,
                  dpi.pcr.tarv_d__all = dpi.pcr.tarv_d_total,
                  dpi.pcr.tarv_n__all = dpi.pcr.tarv_n_total) %>%

    dplyr::filter(partner == ip) %>%

    tidyr::pivot_longer('dpi.colheu.pcr_d__all':'mds.cv.estaveis_n_mds',
                        names_to = c("indicator", "numdenom", "pop_type", "age"),
                        names_sep = "_",
                        values_to = "value") %>%

    dplyr::filter(!numdenom == "prop",
                  !pop_type == "total") %>%

    dplyr::mutate(age = dplyr::recode(age,
                                      "menor2" = "<02 Months",
                                      "0.1" = "<01",
                                      "0.2" = "<02",
                                      "0.4" = "<04",
                                      "1.4" = "01-04",
                                      "5.9" = "05-09",
                                      "10.14" = "10-14",
                                      "15.19" = "15-19",
                                      "0.14" = "<15",
                                      "1.14" = "01-14",
                                      "2.14" = "02-14"),
                  numdenom = dplyr::recode(numdenom,
                                           "n" = "N",
                                           "d" = "D"),
                  pop_type = dplyr::recode(pop_type,
                                           "all" = "All",
                                           "mg" = "MG",
                                           "ml" = "ML",
                                           "mds" = "MDS"),
                  indicator = paste0(indicator,
                                     if_else(numdenom %in% c("D"), "_D", "")),
                  period = {month})

  return(df_cleaned)

}
