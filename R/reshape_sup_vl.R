#' Process quarterly MER supplemental viral load source report
#'
#' @param filename File containing quarterly supplemental MER results
#' @param ip Partner submitting quarterly results
#'
#' @return A tidy dataframe containing TX_PVLS results according to the EPTS Ficha Mestra and EPTS Lab Module
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- reshape_sup_vl()}

reshape_sup_vl <- function(filename, ip){

  df_1 <- readxl::read_excel(filename,
                             sheet = "TX_PVLS_FM",
                             col_types = c("text",
                                           "text", "text", "text", "text", "text",
                                           "numeric", "text", "text", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "text", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "text"),
                             skip = 7) %>%
    dplyr::select(-c(No,
                     contains("reporting"),
                     SISMA_code,
                     contains("Column"))) %>%
    tidyr::pivot_longer(TX_PVLS_D_FM.Total:TX_PVLS_N_FM.T.Prison.all,
                        names_to = "temp",
                        values_to = "value") %>%
    dplyr::filter(!str_detect(temp, "otal"),
                  Partner == ip) %>%
    tidyr::separate(temp,
                    c("indicator", "motive", "group", "age"),
                    sep = "\\.") %>%
    tidyr::separate(indicator,
                    c("indicator"),
                    sep = "_FM") %>%
    dplyr::mutate(motive = dplyr::recode(motive,
                                         "R" = "Routine",
                                         "T" = "Targeted"),
                  age = dplyr::recode(age,
                                      "Less1" = "<01",
                                      "1_4" = "01-04",
                                      "5_9" = "05-09",
                                      "50" = "50+",
                                      "Unk" = "Unknown",
                                      "all" = "All"),
                  age = stringr::str_replace(age, "_", "-"),
                  sex = dplyr::case_when(group == "M" ~ "Male",
                                         group == "F" ~ "Female"),
                  pop_subtype = dplyr::case_when(group == "MG" ~ "PW",
                                                 group == "Lac" ~ "LW"),
                  keypop = dplyr::case_when(group == "PWID" ~ "PWID",
                                            group == "MSM" ~ "MSM",
                                            group == "FSW" ~ "FSW",
                                            group == "Prison" ~ "Prison"),
                  pop_type = dplyr::case_when(
                    (group %in% c("M", "F")) ~ "Age/Sex",
                    (group %in% c("Lac", "MG")) ~ "Pregnant/Breastfeeding",
                    TRUE ~ "KeyPop"),
                  source = "Clinical Module",
                  period = period
    ) %>%
    dplyr::select(-c(group))

  # tidyr::pivot_wider(names_from = indicator, values_from = value)


  df_2 <- read_excel(filename,
                     sheet = "TX_PVLS_LAB",
                     col_types = c("text",
                                   "text", "text", "text", "text", "text",
                                   "numeric", "text", "text", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "text", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "text"),
                     skip = 7) %>%
    dplyr::select(-c(No,
                     contains("reporting"),
                     SISMA_code,
                     contains("Column"))) %>%
    tidyr::pivot_longer(TX_PVLS_D_Lab.Total:TX_PVLS_N_Lab.T.Prison.all,
                        names_to = "temp",
                        values_to = "value") %>%
    dplyr::filter(!stringr::str_detect(temp, "otal"),
                  Partner == ip) %>%
    tidyr::separate(temp,
                    c("indicator", "motive", "group", "age"),
                    sep = "\\.") %>%
    tidyr::separate(indicator,
                    c("indicator"),
                    sep = "_Lab") %>%
    dplyr::mutate(motive = dplyr::recode(motive,
                                         "R" = "Routine",
                                         "T" = "Targeted"),
                  age = dplyr::recode(age,
                                      "Less1" = "<01",
                                      "1_4" = "01-04",
                                      "5_9" = "05-09",
                                      "50" = "50+",
                                      "Unk" = "Unknown",
                                      "all" = "All"),
                  age = replace_na(age, "Unknown"),
                  age = stringr::str_replace(age, "_", "-"),
                  sex = dplyr::case_when(group == "M" ~ "Male",
                                         group == "F" ~ "Female"),
                  pop_subtype = dplyr::case_when(group == "MG" ~ "PW",
                                                 group == "Lac" ~ "LW"),
                  keypop = dplyr::case_when(group == "PWID" ~ "PWID",
                                            group == "MSM" ~ "MSM",
                                            group == "FSW" ~ "FSW",
                                            group == "Prison" ~ "Prison"),
                  pop_type = dplyr::case_when(
                    (group %in% c("M", "F")) ~ "Age/Sex",
                    (group %in% c("Lac", "MG")) ~ "Pregnant/Breastfeeding",
                    TRUE ~ "KeyPop"),
                  source = "Lab Module",
                  period = period
    ) %>%
    dplyr::select(-c(group))

  # tidyr::pivot_wider(names_from = indicator, values_from = value)

  df <-  dplyr::bind_rows(df_1, df_2) %>%
    dplyr::filter(value > 0) %>%
    dplyr::select(snu = Province,
                  psnu = District,
                  sitename = `Health Facility`,
                  datim_uid = DATIM_code,
                  period,
                  indicator,
                  pop_type,
                  motive,
                  age,
                  sex,
                  pop_subtype,
                  keypop,
                  source,
                  value)

}
