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

      sub_group = dplyr::case_when(str_detect(indicator, "filhos_10") ~ "Filhos <10",
                                   str_detect(indicator, "parceiro") ~ "Parceiro",
                                   str_detect(indicator, "mae_pai") ~ "Mae/Pai"),

      result_status = dplyr::case_when(stringr::str_detect(indicator, "negativo")  ~ "Negativo",
                                       stringr::str_detect(indicator, "positivo")  ~ "Positivo"),

      age_coarse = dplyr::case_when(str_detect(indicator, "filhos_10") ~ "<15",
                                    str_detect(indicator, "parceiro") ~ "15+",
                                    str_detect(indicator, "mae_pai") ~ "15+"),

      indicator = dplyr::case_when(str_detect(indicator, "teste_de_subgrupo") ~ "ATS_CI_TST",
                                   str_detect(indicator, "contactos_de_casos_de_indice") ~ "ATS_CI",
                                   str_detect(indicator, "numero_diagnosticado") ~ "ATS_LIG_DEN",
                                   str_detect(indicator, "ligado_aos") ~ "ATS_LIG_NUM"),


      source = "LdR ATS",

      age = NA_character_,

      sex = NA_character_)


  df_pos <- df_all %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = dplyr::case_when(indicator == "ATS_CI_TST" ~ "ATS_CI_TST_POS"))


  df_parse <- dplyr::bind_rows(df_all, df_pos) %>%
    dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator, source, modality, modality_sub, sub_group, sex, age_coarse, age, result_status, value)


  return(df_parse)

}
