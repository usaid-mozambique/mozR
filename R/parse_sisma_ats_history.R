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
#'  df <- parse_sisma_ats_history()}

parse_sisma_ats_history <- function(file) {

  df_parse <- file %>%

    dplyr::mutate(
      modality = dplyr::case_when(stringr::str_detect(indicator, "Banco de Socorros") ~ "ATS-BdS",
                                  stringr::str_detect(indicator, "Consultas Externas") ~ "ATS-CE",
                                  stringr::str_detect(indicator, "Enfermaria") ~ "ATS-Enf",
                                  stringr::str_detect(indicator, "Outro ATIP") ~ "ATS-ATIP Outro",
                                  stringr::str_detect(indicator, "SMI") ~ "ATS-SMI",
                                  stringr::str_detect(indicator, "TB") ~ "ATS-TB",
                                  stringr::str_detect(indicator, "Triagem") ~ "ATS-Triagem",
                                  stringr::str_detect(indicator, "UATS") ~ "ATS-UATS",
                                  stringr::str_detect(indicator, "ATS-C") ~ "ATS-C"),

      modality_sub = NA_character_,

      sub_group = dplyr::case_when(stringr::str_detect(indicator, "- MTS") ~ "MTS",
                                   stringr::str_detect(indicator, "- PID") ~ "PID",
                                   stringr::str_detect(indicator, "- HSH") ~ "HSH",
                                   stringr::str_detect(indicator, "- MIN") ~ "MIN",
                                   stringr::str_detect(indicator, "- REC") ~ "REC"),

      indicator = dplyr::case_when(stringr::str_detect(indicator, "1 vez testado") ~ "ATS_HIST_PRIM",
                                   stringr::str_detect(indicator, "pos. ") ~ "ATS_HIST_POS",
                                   stringr::str_detect(indicator, "Subgrupo") ~ "ATS_KP"),

      result_status = dplyr::case_when(stringr::str_detect(indicator, "Positi") ~ "Positive",
                                       stringr::str_detect(indicator, "Negativ") ~ "Negative"),

      source = "LdR ATS",

      age_coarse = NA_character_,

      age = NA_character_,

      sex = NA_character_) %>%

    dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator, source, modality, modality_sub, sub_group, sex, age_coarse, age, value)


  return(df_parse)

}
