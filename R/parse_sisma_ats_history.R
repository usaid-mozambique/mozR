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
      modality = dplyr::case_when(stringr::str_detect(indicator, "Banco de Socorros") ~ "ER",
                                  stringr::str_detect(indicator, "Consultas Externas") ~ "Outpatient",
                                  stringr::str_detect(indicator, "Enfermaria") ~ "Inpatient",
                                  stringr::str_detect(indicator, "Outro ATIP") ~ "Other PICT",
                                  stringr::str_detect(indicator, "SMI") ~ "MCH",
                                  stringr::str_detect(indicator, "TB") ~ "TB",
                                  stringr::str_detect(indicator, "Triagem") ~ "Triage",
                                  stringr::str_detect(indicator, "UATS") ~ "VCT",
                                  stringr::str_detect(indicator, "ATS-C") ~ "Community"),

      sub_group = dplyr::case_when(stringr::str_detect(indicator, "- MTS") ~ "FSW",
                                   stringr::str_detect(indicator, "- PID") ~ "IDU",
                                   stringr::str_detect(indicator, "- HSH") ~ "MSM",
                                   stringr::str_detect(indicator, "- MIN") ~ "Miners",
                                   stringr::str_detect(indicator, "- REC") ~ "Prisoners"),

      indicator = dplyr::case_when(stringr::str_detect(indicator, "1 vez testado") ~ "HTS_HIST_FIRST",
                                   stringr::str_detect(indicator, "pos. ") ~ "HTS_HIST_POS",
                                   stringr::str_detect(indicator, "Subgrupo") ~ "HTS_KP"),

      result_status = dplyr::case_when(stringr::str_detect(indicator, "Positi") ~ "Positive",
                                       stringr::str_detect(indicator, "Negativ") ~ "Negative"),

      source = "HTS Register",

      age_coarse = NA_character_,
      age_semi_fine = NA_character_,
      sex = NA_character_) %>%

    dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator, source, modality, sub_group, sex, age_coarse, age_semi_fine, result_status, value)


  return(df_parse)

}
