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

  df_all <- file %>%

    dplyr::filter(!is.na(value)) %>%

    dplyr::mutate(
      indicator = stringr::str_remove_all(indicator, "mz_ats_"),

      modality = dplyr::case_when(stringr::str_detect(indicator, "uats")                ~ "ATS-UATS",
                                  stringr::str_detect(indicator, "consultas_externas")  ~ "ATS-CE",
                                  stringr::str_detect(indicator, "triagem")             ~ "ATS-Triagem",
                                  stringr::str_detect(indicator, "enfermaria")          ~ "ATS-Enf",
                                  stringr::str_detect(indicator, "outro_atip")          ~ "ATS-ATIP Outro",
                                  stringr::str_detect(indicator, "banco_de_socorros")   ~ "ATS-BdS",
                                  stringr::str_detect(indicator, "_smi")                ~ "ATS-SMI",
                                  stringr::str_detect(indicator, "_tb")                 ~ "ATS-TB",
                                  stringr::str_detect(indicator, "_ats_c")              ~ "ATS-C"),

      modality_sub = NA_character_,

      sub_group = dplyr::case_when(stringr::str_detect(indicator, "_mts_") ~ "MTS",
                                   stringr::str_detect(indicator, "_pid_") ~ "PID",
                                   stringr::str_detect(indicator, "_hsh_") ~ "HSH",
                                   stringr::str_detect(indicator, "_min_") ~ "MIN",
                                   stringr::str_detect(indicator, "_rec_") ~ "REC"),

      result_status = dplyr::case_when(stringr::str_detect(indicator, "ositi")   ~ "Positivo",
                                       stringr::str_detect(indicator, "egativ")  ~ "Negativo"),

      indicator = dplyr::case_when(stringr::str_detect(indicator, "_1_vez_testado")    ~ "ATS_HIST_PRIM",
                                   stringr::str_detect(indicator, "_pos_no_passado_")  ~ "ATS_HIST_POS",
                                   stringr::str_detect(indicator, "teste_de_subgrupo") ~ "ATS_KP"),

      age_coarse = NA_character_,

      source = "LdR ATS",

      age = NA_character_,

      sex = NA_character_)


  df_pos <- df_all %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = dplyr::case_when(indicator == "ATS_KP" ~ "ATS_KP_POS"))


  df_parse <- dplyr::bind_rows(df_all, df_pos) %>%
    dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator, source, modality, modality_sub, sub_group, sex, age_coarse, age, result_status, value)


  return(df_parse)

}
