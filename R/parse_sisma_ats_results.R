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

  df_all <- file %>%

    dplyr::filter(!is.na(value)) %>%

    dplyr::mutate(indicator = str_remove(indicator, "MZ ATS - Resultado por grupo etario - ")) %>%

    tidyr::separate(indicator, c("modality", "age", "sex"), sep = ", ") %>%

    dplyr::mutate(
      age = stringr::str_remove_all(age, " |anos|ano"),

      sex = dplyr::case_when(sex == "FEMININO"   ~ "Feminino",
                             sex == "MASCULINO" ~ "Masculino"),
      age = dplyr::case_when(age == "<1"                    ~ "<01",
                             age == "1-9"                   ~ "01-09",
                             stringr::str_detect(age, "50") ~ "50+",
                             TRUE ~ age),

      result_status = stringr::str_extract(modality, "Negativo|Positivo"),

      result_status = dplyr::case_when(result_status == "Negativo" ~ "Negativo",
                                       result_status == "Positivo" ~ "Positivo"),

      modality = stringr::str_remove(modality, " (Negativo|Positivo)"),
      modality = dplyr::case_when(modality == "ATS-C"               ~ "ATS-C",
                                  modality == "Banco de Socorros"   ~ "ATS-BdS",
                                  modality == "Consultas Externas"  ~ "ATS-CE",
                                  modality == "Enfermaria"          ~ "ATS-Enf",
                                  modality == "Outro ATIP"          ~ "ATS-ATIP Outro",
                                  modality == "SMI"                 ~ "ATS-SMI",
                                  modality == "TB"                  ~ "ATS-TB",
                                  modality == "Triagem"             ~ "ATS-Triagem",
                                  modality == "UATS"                ~ "ATS-UATS"),

      modality_sub = NA_character_,

      source = "LdR ATS",

      age_coarse = dplyr::case_when(age == "<01"   ~ "<15",
                                    age == "01-09" ~ "<15",
                                    age == "10-14" ~ "<15"),
      age_coarse = tidyr::replace_na(age_coarse, "15+"),

      sub_group = NA_character_,

      indicator = "ATS_TST")


  df_pos <- df_all %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = dplyr::case_when(indicator == "ATS_TST" ~ "ATS_TST_POS"))


  df_parse <- dplyr::bind_rows(df_all, df_pos) %>%
    dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator, source, modality, modality_sub, sub_group, sex, age_coarse, age, result_status, value)

  return(df_parse)

}
