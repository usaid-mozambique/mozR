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
#'  df <- parse_sisma_ats_index()}

parse_sisma_ats_index <- function(file) {

  df_parse <- file %>%

    dplyr::mutate(
      modality = dplyr::case_when(str_detect(indicator, "Banco de Socorros") ~ "ER",
                                  str_detect(indicator, "Consultas Externas") ~ "Outpatient",
                                  str_detect(indicator, "Enfermaria") ~ "Inpatient",
                                  str_detect(indicator, "Outro ATIP") ~ "Other PICT",
                                  str_detect(indicator, "SMI") ~ "MCH",
                                  str_detect(indicator, "TB") ~ "TB",
                                  str_detect(indicator, "Triagem") ~ "Triage",
                                  str_detect(indicator, "UATS") ~ "VCT",
                                  str_detect(indicator, "ATS-C") ~ "Community"),

      sub_group = dplyr::case_when(str_detect(indicator, "Filhos <10") ~ "Chilren <10",
                                   str_detect(indicator, "Parceiro") ~ "Partner",
                                   str_detect(indicator, " / Pai ") ~ "Mother/Father"),

      indicator = dplyr::case_when(str_detect(indicator, "Teste de Subgrupo") ~ "HTS_CI_TST",
                                   str_detect(indicator, "Contactos de casos de indice") ~ "HTS_CI",
                                   str_detect(indicator, "Numero Diagnosticado") ~ "HTS_LIG_DEN",
                                   str_detect(indicator, "ligado aos") ~ "HTS_LIG_NUM"),

      result_status = dplyr::case_when(str_detect(indicator, "Positi") ~ "Positive",
                                       str_detect(indicator, "Negativ") ~ "Negative"),

      age_coarse = dplyr::case_when(str_detect(indicator, "Filhos <10") ~ "<15",
                                    str_detect(indicator, "Parceiro") ~ "15+",
                                    str_detect(indicator, " / Pai ") ~ "15+"),

      source = "HTS Register",

      age_semi_fine = NA_character_,

      sex = NA_character_) %>%

    dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator, source, modality, sub_group, sex, age_coarse, age_semi_fine, result_status, value)


  return(df_parse)

}
