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

                  result_status = dplyr::case_when(stringr::str_detect(indicator, "ositivo") ~ "Positive",
                                                   stringr::str_detect(indicator, "egativo") ~ "Negative",
                                                   stringr::str_detect(indicator, "ndeter") ~ "Indet."),

                  sex = dplyr::case_when(stringr::str_detect(indicator, "Crian") ~ "Unknown",
                                         TRUE ~ "Female"),

                  modality = dplyr::case_when(stringr::str_detect(indicator, "SMI CCS") ~ "SMI-CCS",
                                              stringr::str_detect(indicator, "SMI CCD") ~ "SMI-CCD"),

                  modality_sub = dplyr::case_when(stringr::str_detect(indicator, "osto") ~ "Fixed Post",
                                                  stringr::str_detect(indicator, "rigad") ~ "Mobile Brigade"),

                  source = "MCH Register",

                  sub_group = NA_character_,

                  age_semi_fine = NA_character_,

                  indicator = "HTS_TST")

  df_pos <- df_all %>%
    dplyr::filter(result_status == "Positive") %>%
    dplyr::mutate(indicator = dplyr::case_when(indicator == "HTS_TST" ~ "HTS_TST_POS"))


  df_parse <- dplyr::bind_rows(df_all, df_pos) %>%
    dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator, source, modality, modality_sub, sub_group, sex, age_coarse, age_semi_fine, result_status, value)

  return(df_parse)

}
