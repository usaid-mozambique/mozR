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
#'  df <- parse_sisma_ats_results()}

parse_sisma_ats_results <- function(file) {

  df_parse <- df %>%
    dplyr::mutate(indicator = str_remove(indicator, "MZ ATS - Resultado por grupo etario - ")) %>%

    tidyr::separate(indicator, c("modality", "age_semi_fine", "sex"), sep = ", ") %>%

    dplyr::mutate(
      age_semi_fine = stringr::str_remove_all(age_semi_fine, " |anos|ano"),
      sex = dplyr::case_when(sex == "FEMININO" ~ "Female",
                             sex == "MASCULINO" ~ "Male"),
      age_semi_fine = dplyr::case_when(age_semi_fine == "<1" ~ "<01",
                                       age_semi_fine == "1-9" ~ "01-09",
                                       TRUE ~ age_semi_fine),
      result_status = stringr::str_extract(modality, "Negativo|Positivo"),
      result_status = dplyr::case_when(result_status == "Negativo" ~ "Negative",
                                       result_status == "Positivo" ~ "Positive"),
      modality = stringr::str_remove(modality, " (Negativo|Positivo)"),
      modality = dplyr::case_when(modality == "ATS-C" ~ "Community",
                                  modality == "Banco de Socorros" ~ "ER",
                                  modality == "Consultas Externas" ~ "Outpatient",
                                  modality == "Enfermaria" ~ "Inpatient",
                                  modality == "Outro ATIP" ~ "Other PICT",
                                  modality == "SMI" ~ "MCH",
                                  modality == "TB" ~ "TB",
                                  modality == "Triagem" ~ "Triage",
                                  modality == "UATS" ~ "VCT"),
      age_coarse = dplyr::case_when(age_semi_fine == "<01" ~ "<15",
                                    age_semi_fine == "01-09" ~ "<15",
                                    age_semi_fine == "10-14" ~ "<15"),
      age_coarse = tidyr::replace_na(age_coarse, "15+"),
      sub_group = NA_character_,
      indicator = "HTS_TST") %>%

    dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator, modality, sub_group, sex, age_coarse, age_semi_fine, result_status, value)

  return(df_parse)

}
