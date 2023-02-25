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

    dplyr::mutate(
      indicator = stringr::str_remove_all(indicator, "mz_ats_resultado_por_grupo_etario_"),

      sex = dplyr::case_when(stringr::str_detect(indicator, "feminino")  ~ "Feminino",
                             stringr::str_detect(indicator, "masculino") ~ "Masculino"),

      age = dplyr::case_when(stringr::str_detect(indicator, "_1_ano")  ~ "<01",
                             stringr::str_detect(indicator, "1_9_ano") ~ "01-09",
                             stringr::str_detect(indicator, "10_14")   ~ "10-14",
                             stringr::str_detect(indicator, "15_19")   ~ "15-19",
                             stringr::str_detect(indicator, "20_24")   ~ "20-24",
                             stringr::str_detect(indicator, "25_49")   ~ "25-49",
                             stringr::str_detect(indicator, "_50_")    ~ "50+"),

      result_status = dplyr::case_when(stringr::str_detect(indicator, "negativo")  ~ "Negativo",
                                       stringr::str_detect(indicator, "positivo")  ~ "Positivo"),

      modality = dplyr::case_when(stringr::str_detect(indicator, "uats")                ~ "ATS-UATS",
                                  stringr::str_detect(indicator, "consultas_externas")  ~ "ATS-CE",
                                  stringr::str_detect(indicator, "triagem")             ~ "ATS-Triagem",
                                  stringr::str_detect(indicator, "enfermaria")          ~ "ATS-Enf",
                                  stringr::str_detect(indicator, "outro_atip")          ~ "ATS-ATIP Outro",
                                  stringr::str_detect(indicator, "banco_de_socorros")   ~ "ATS-BdS",
                                  stringr::str_detect(indicator, "smi_")                ~ "ATS-SMI",
                                  stringr::str_detect(indicator, "tb_")                 ~ "ATS-TB",
                                  stringr::str_detect(indicator, "ats_c")               ~ "ATS-C"),

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
