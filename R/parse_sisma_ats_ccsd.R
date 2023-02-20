#' A specific helper function for parsing a cleaned CSV export from SISMA
#'
#' @param file Dataframe cleaned via reshape_sisma
#'
#' @return A tidy format of SISMA dataframe
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_ats_ccsd()}

parse_sisma_ats_ccsd <- function(file) {

  df_all <- file %>%
    dplyr::filter(!str_detect(indicator, "estada")) %>%

    dplyr::mutate(age_coarse = dplyr::case_when(stringr::str_detect(indicator, "Crian") ~ "<15",
                                                TRUE ~ "15+"),

                  result_status = dplyr::case_when(stringr::str_detect(indicator, "ositivo") ~ "Positivo",
                                                   stringr::str_detect(indicator, "egativo") ~ "Negativo",
                                                   stringr::str_detect(indicator, "ndeter") ~ "Indet."),

                  sex = dplyr::case_when(stringr::str_detect(indicator, "Crian") ~ "Desconh.",
                                         TRUE ~ "Feminino"),

                  modality = dplyr::case_when(stringr::str_detect(indicator, "SMI CCS") ~ "SMI-CCS",
                                              stringr::str_detect(indicator, "SMI CCD") ~ "SMI-CCD"),

                  modality_sub = dplyr::case_when(stringr::str_detect(indicator, "osto") ~ "Posto Fixo",
                                                  stringr::str_detect(indicator, "rigad") ~ "Brigada Movel"),

                  source = "LdR SMI",

                  sub_group = NA_character_,

                  age = NA_character_,

                  indicator = "ATS_TST")

  df_pos <- df_all %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = dplyr::case_when(indicator == "ATS_TST" ~ "ATS_TST_POS"))


  df_parse <- dplyr::bind_rows(df_all, df_pos) %>%
    dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator, source, modality, modality_sub, sub_group, sex, age_coarse, age, result_status, value)

  return(df_parse)

}
