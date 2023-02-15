#' Process monthly DISA results for Viral Load
#' @param filename Local path to the monthly DISA dataset
#' @return A tidy dataframe with monthly DISA results
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- process_disa_vl()}


process_disa_vl <- function(filename) {

  # ingestion
  df_age <- readxl::read_excel(filename,
                               sheet = "Age & Sex",
                               col_types = c("text",
                                             "text", "text", "text", "text", "text",
                                             "text", "text", "text", "numeric",
                                             "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric",
                                             "numeric"),
                               skip = 4)


  df_pw <- readxl::read_excel(filename,
                              sheet = "S. Viral (M. Gravidas)",
                              col_types = c("text",
                                            "text", "text", "text", "text", "text",
                                            "text", "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric"),
                              skip = 4)


  df_lw <- readxl::read_excel(filename,
                              sheet = "S. Viral (M. Lactantes)",
                              col_types = c("text",
                                            "text", "text", "text", "text", "text",
                                            "text", "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric"),
                              skip = 4)


  df_tat <- readxl::read_excel(filename,
                               sheet = "TRL - AVG",
                               col_types = c("text",
                                             "text", "text", "text", "text", "text",
                                             "text", "numeric", "numeric", "numeric",
                                             "numeric", "numeric"),
                               skip = 4)


  # tidy
  df_all <- dplyr::bind_rows(df_age, df_pw, df_lw, df_tat) %>%

    dplyr::select(!c(datim_uid, sitetype, tidyselect::contains("total"))) %>%

    tidyr::pivot_longer(cols = tidyselect::starts_with("vl"),
                        names_to = c("indicator", "group", "motive", "result", "tat_step"),
                        names_sep = "_",
                        values_to = "value") %>%

    dplyr::filter(value > 0)


  # feature engineering
  df_final <- df_all %>%
    dplyr::mutate(period = as.Date(month, "%Y-%m-%d"),

                  sex = dplyr::case_when(indicator == "vl" & sex == "F" ~ "Female",
                                         indicator == "vl" & sex == "M" ~ "Male",
                                         indicator == "vl" & sex %in% c("UNKNOWN", "Not Specified", "N\\u00e3o especificado") ~ "Unknown"),

                  age = dplyr::case_when(age == "<1" ~ "<01",
                                         age == "NS" ~ "Unknown Age",
                                         stringr::str_detect(age, "pecif") ~ "Unknown Age",
                                         TRUE ~ age),

                  group = dplyr::case_when(group == "age" ~ "Age",
                                           group == "pw" ~ "PW",
                                           group == "lw" ~ "LW"),

                  motive = dplyr::case_when(motive == "routine" ~ "Routine",
                                            motive == "failure" ~ "Theraputic Failure",
                                            motive == "repeat" ~ "Post Breastfeeding",
                                            motive == "unspecified" ~ "Not Specified",
                                            motive == "" ~ NA_character_),

                  tat_step = dplyr::case_when(str_detect(tat_step, "s1.") ~ "S1: Collection to Receipt",
                                              str_detect(tat_step, "s2.") ~ "S2: Receipt to Registration",
                                              str_detect(tat_step, "s3.") ~ "S3: Registration to Analysis",
                                              str_detect(tat_step, "s4.") ~ "S4: Analysis to Validation"),

                  VL = dplyr::case_when(result == "suppress" | result == "unsuppress" & indicator == "vl" ~ value),

                  VLS = dplyr::case_when(result == "suppress" & indicator == "vl" ~ value),

                  VLS = dplyr::case_when(indicator == "vl" ~replace_na(VLS, 0)),

                  TAT = dplyr::case_when(indicator == "vltat" ~ value)) %>%

    dplyr::select(!c(result, indicator, value))

  return(df_final)

}
