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
#'  df <- parse_sisma_ats_saaj_cm()}

parse_sisma_ats_saaj_cm <- function(file) {

  df_all <- file %>%

    dplyr::mutate(age = dplyr::case_when(stringr::str_detect(indicator, "<=10")    ~ "<10",   # saaj ages
                                         stringr::str_detect(indicator, "10 - 14") ~ "10-14",
                                         stringr::str_detect(indicator, "15 - 19") ~ "15-19",
                                         stringr::str_detect(indicator, "20 - 24") ~ "20-24",
                                         stringr::str_detect(indicator, ">=25")    ~ "25+",

                                         stringr::str_detect(indicator, "15-19")   ~ "15-19", # cm ages
                                         stringr::str_detect(indicator, "20-24")   ~ "20-24",
                                         stringr::str_detect(indicator, "25 - 29") ~ "25-29",
                                         stringr::str_detect(indicator, "30 - 34") ~ "30-34",
                                         stringr::str_detect(indicator, "35 - 39") ~ "35-39",
                                         stringr::str_detect(indicator, " 49")     ~ "40-49",
                                         stringr::str_detect(indicator, "50 anos") ~ "50+"),

                  age_coarse = dplyr::case_when(age %in% c("<10", "10-14") ~ "<15",
                                                TRUE ~ "15+"),

                  sex = dplyr::case_when(stringr::str_detect(indicator, "FEMININO") ~ "Female",
                                         stringr::str_detect(indicator, "MASCULINO") ~ "Male",
                                         stringr::str_detect(indicator, "MZ C.MASC") ~ "Male"),

                  modality = dplyr::case_when(stringr::str_detect(indicator, "MZ SAAJ") ~ "SAAJ",
                                              stringr::str_detect(indicator, "MZ C.MASC") ~ "CM"),

                  result_status = dplyr::case_when(stringr::str_detect(indicator, "ositiv") ~ "Positive",
                                                   stringr::str_detect(indicator, "egativ") ~ "Negative",
                                                   stringr::str_detect(indicator, "ndeter") ~ "Indet."),

                  modality_sub = NA_character_,

                  source = dplyr::case_when(stringr::str_detect(indicator, "MZ SAAJ") ~ "SAAJ Register",
                                            stringr::str_detect(indicator, "MZ C.MASC") ~ "CM Register"),

                  sub_group = dplyr::case_when(stringr::str_detect(indicator, "arceiro") ~ "Partner",
                                               TRUE ~ NA_character_),

                  indicator = "HTS_TST")


  df_pos <- df_all %>%
    dplyr::filter(result_status == "Positive") %>%
    dplyr::mutate(indicator = dplyr::case_when(indicator == "HTS_TST" ~ "HTS_TST_POS"))


  df_parse <- dplyr::bind_rows(df_all, df_pos) %>%
    dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator, source, modality, modality_sub, sub_group, sex, age_coarse, age, result_status, value)

  return(df_parse)

}
