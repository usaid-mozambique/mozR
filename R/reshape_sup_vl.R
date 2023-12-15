#' Process quarterly MER supplemental viral load source report
#'
#' @param filename File containing quarterly supplemental MER results
#'
#' @return A tidy dataframe containing TX_PVLS results according to the EPTS Ficha Mestra and EPTS Lab Module
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- reshape_sup_vl()}

reshape_sup_vl <- function(filename){


  ip_temp <- extract_em_meta(filename, type = "ip")

  month_temp <- extract_em_meta(filename, type = "month") %>%
    stringr::str_replace("FY", "20")


  df_1 <- readxl::read_excel(filename,
                             sheet = "TX_PVLS_FM",
                             skip = 7) %>%

    dplyr::select(-c(No,
                     contains("reporting"),
                     SISMA_code,
                     contains("otal"),
                     contains("Column"))) %>%

    dplyr::rename(partner = Partner,
                  snu = Province,
                  psnu = District,
                  sitename = `Health Facility`,
                  datim_uid = DATIM_code) %>%

    dplyr::filter(partner == ip_temp) %>%

    dplyr::mutate(dplyr::across(TX_PVLS_D_FM.R.M.Less1:TX_PVLS_N_FM.T.Prison.all, as.numeric)) %>%

    tidyr::pivot_longer(!c(partner, snu, psnu, sitename, datim_uid),
                        names_to = "temp",
                        values_to = "value") %>%

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
                                      "65" = "65+",
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
                  period = month_temp
    ) %>%

    dplyr::select(-c(group))




  df_2 <- read_excel(filename,
                     sheet = "TX_PVLS_LAB",
                     skip = 7) %>%

    dplyr::select(-c(No,
                     contains("reporting"),
                     SISMA_code,
                     contains("otal"),
                     contains("Column"))) %>%

    dplyr::rename(partner = Partner,
                  snu = Province,
                  psnu = District,
                  sitename = `Health Facility`,
                  datim_uid = DATIM_code) %>%

    dplyr::filter(partner == ip_temp) %>%

    dplyr::mutate(dplyr::across(TX_PVLS_D_Lab.R.M.Less1:TX_PVLS_N_Lab.T.Prison.all, as.numeric)) %>%

    tidyr::pivot_longer(!c(partner, snu, psnu, sitename, datim_uid),
                        names_to = "temp",
                        values_to = "value") %>%

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
                                      "65" = "65+",
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
                  period = month_temp
    ) %>%

    dplyr::select(-c(group))


  df <-  dplyr::bind_rows(df_1, df_2) %>%
    dplyr::filter(value > 0) %>%
    dplyr::select(snu,
                  psnu,
                  sitename,
                  datim_uid,
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


  return(df)

}

# REVISE FOR FY24Q1
# reshape_sup_vl <- function(filename){
#
#
#   ip_temp <- extract_em_meta(filename, type = "ip")
#
#   month_temp <- extract_em_meta(filename, type = "month") %>%
#     stringr::str_replace("FY", "20")
#
#
#   df_1 <- readxl::read_excel(filename,
#                              sheet = "TX_PVLS_FM",
#                              skip = 7) %>%
#
#     dplyr::select(-c(No, # change
#                      contains("eport"),
#                      contains("emove"),
#                      contains("eriod"),
#                      contains("otal"),
#                      contains("Column"))) %>%
#
#     dplyr::rename(partner = Partner,
#                   snu = Province,
#                   psnu = District,
#                   sitename = `Health Facility`,
#                   datim_uid = DATIM_code) %>%
#
#     dplyr::filter(partner == ip_temp) %>%
#
#     dplyr::mutate(dplyr::across(TX_PVLS_D_FM..M.Less1:TX_PVLS_N_FM..Prisioners.all, as.numeric)) %>%
#
#     tidyr::pivot_longer(!c(partner, snu, psnu, sitename, datim_uid),
#                         names_to = "temp",
#                         values_to = "value") %>%
#
#     tidyr::separate(temp,
#                     c("indicator", "motive", "group", "age"),
#                     sep = "\\.") %>%
#
#     tidyr::separate(indicator,
#                     c("indicator"),
#                     sep = "_FM") %>%
#
#     dplyr::mutate(motive = dplyr::recode(motive,
#                                          "R" = "Routine",
#                                          "T" = "Targeted"),
#                   age = dplyr::recode(age,
#                                       "Less1" = "<01",
#                                       "1_4" = "01-04",
#                                       "5_9" = "05-09",
#                                       "65" = "65+",
#                                       "Unk" = "Unknown",
#                                       "all" = "All"),
#                   age = stringr::str_replace(age, "_", "-"),
#                   sex = dplyr::case_when(group == "M" ~ "Male",
#                                          group == "F" ~ "Female"),
#                   pop_subtype = dplyr::case_when(group == "MG" ~ "PW",
#                                                  group == "Lac" ~ "LW"),
#                   keypop = dplyr::case_when(group == "PWID" ~ "PWID",
#                                             group == "MSM" ~ "MSM",
#                                             group == "FSW" ~ "FSW",
#                                             group == "Prison" ~ "Prison"),
#                   pop_type = dplyr::case_when(
#                     (group %in% c("M", "F")) ~ "Age/Sex",
#                     (group %in% c("Lac", "MG")) ~ "Pregnant/Breastfeeding",
#                     TRUE ~ "KeyPop"),
#                   source = "Clinical Module",
#                   period = month_temp
#     ) %>%
#
#     dplyr::select(-c(group))
#
#
#
#
#   df_2 <- read_excel(filename,
#                      sheet = "TX_PVLS_LAB",
#                      skip = 7) %>%
#
#     dplyr::select(-c(No, # change
#                      contains("eport"),
#                      contains("emove"),
#                      contains("eriod"),
#                      contains("otal"),
#                      contains("Column"))) %>%
#
#     dplyr::rename(partner = Partner,
#                   snu = Province,
#                   psnu = District,
#                   sitename = `Health Facility`,
#                   datim_uid = DATIM_code) %>%
#
#     dplyr::filter(partner == ip_temp) %>%
#
#     dplyr::mutate(dplyr::across(TX_PVLS_D_Lab..M.Less1:TX_PVLS_N_Lab..Prisioners.all, as.numeric)) %>%
#
#     tidyr::pivot_longer(!c(partner, snu, psnu, sitename, datim_uid),
#                         names_to = "temp",
#                         values_to = "value") %>%
#
#     tidyr::separate(temp,
#                     c("indicator", "motive", "group", "age"),
#                     sep = "\\.") %>%
#
#     tidyr::separate(indicator,
#                     c("indicator"),
#                     sep = "_Lab") %>%
#
#     dplyr::mutate(motive = dplyr::recode(motive,
#                                          "R" = "Routine",
#                                          "T" = "Targeted"),
#                   age = dplyr::recode(age,
#                                       "Less1" = "<01",
#                                       "1_4" = "01-04",
#                                       "5_9" = "05-09",
#                                       "65" = "65+",
#                                       "Unk" = "Unknown",
#                                       "all" = "All"),
#                   age = replace_na(age, "Unknown"),
#                   age = stringr::str_replace(age, "_", "-"),
#                   sex = dplyr::case_when(group == "M" ~ "Male",
#                                          group == "F" ~ "Female"),
#                   pop_subtype = dplyr::case_when(group == "MG" ~ "PW",
#                                                  group == "Lac" ~ "LW"),
#                   keypop = dplyr::case_when(group == "PWID" ~ "PWID",
#                                             group == "MSM" ~ "MSM",
#                                             group == "FSW" ~ "FSW",
#                                             group == "Prison" ~ "Prison"),
#                   pop_type = dplyr::case_when(
#                     (group %in% c("M", "F")) ~ "Age/Sex",
#                     (group %in% c("Lac", "MG")) ~ "Pregnant/Breastfeeding",
#                     TRUE ~ "KeyPop"),
#                   source = "Lab Module",
#                   period = month_temp
#     ) %>%
#
#     dplyr::select(-c(group))
#
#
#   df <-  dplyr::bind_rows(df_1, df_2) %>%
#     dplyr::filter(value > 0) %>%
#     dplyr::select(snu,
#                   psnu,
#                   sitename,
#                   datim_uid,
#                   period,
#                   indicator,
#                   pop_type,
#                   motive,
#                   age,
#                   sex,
#                   pop_subtype,
#                   keypop,
#                   source,
#                   value)
#
#
#   return(df)
#
# }
