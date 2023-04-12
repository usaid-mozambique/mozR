#' Process monthly enhanced monitoring IMER submission from PEPFAR Mozambique Clinical Partners
#' @param filename Local path to the monthly IP submission
#' @return A tidy dataframe with monthly enhanced monitoring IMER results
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- reshape_em_imer()}

reshape_em_imer <- function(filename){

  ip_temp <- extract_em_meta(filename, type = "ip")
  month_temp <- extract_em_meta(filename, type = "month")

  df <- readxl::read_excel(filename,
                           sheet = "TX NEW, TX CURR AND IMER",
                           skip = 8,
                           col_types = "text") %>%

    tidyr::pivot_longer(TX_NEWTot:I4_ER4_40_RetCalc,
                        names_to = "indicator",
                        values_to = "value") %>%

    dplyr::select(partner = Partner,
                  snu = Province,
                  psnu = District,
                  sitename = `Health Facility`,
                  datim_uid = DATIM_code,
                  indicator,
                  value) %>%

    dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
    dplyr::filter(!indicator_new == "remove") %>%
    tidyr::separate(indicator_new,
                    c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                    sep = "\\.") %>%
    dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                  value = as.numeric(value),
                  period = month_temp,
                  indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                  age = stringr::str_replace_all(age, "\\_", "-"),
                  age = dplyr::recode(age,
                                      `<1`  = "<01",
                                      `1-4` = "01-04",
                                      `5-9` = "05-09",
                                      "unknown" = "Unknown Age"), # age coding correction
                  sex = dplyr::recode(sex,
                                      "M" = "Male",
                                      "F" = "Female"),
                  key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                             pop_type == "MSM" ~ "MSM",
                                             pop_type == "PWID" ~ "PWID",
                                             pop_type == "PPCS" ~ "PPCS"),
                  pop_type = dplyr::recode(pop_type,
                                           "FSW" = "KP",
                                           "MSM" = "KP",
                                           "PWID" = "KP",
                                           "PPCS" = "KP"),
                  pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                              TRUE ~ pop_type),
                  numdenom = tidyr::replace_na(numdenom, "N"),
                  er_status = dplyr::recode(er_status,
                                            "Initiated ART" = NA_character_)) %>%
    dplyr::filter(partner == ip_temp) %>%
    dplyr::select(partner,
                  snu,
                  psnu,
                  sitename,
                  datim_uid,
                  period,
                  indicator,
                  sex,
                  age,
                  pop_type,
                  key_pop,
                  dispensation,
                  numdenom,
                  er_status,
                  dsd_eligibility,
                  value)

  tx_curr_prev <- df %>%
    dplyr::filter(indicator == "TX_CURR") %>%
    dplyr::mutate(indicator = dplyr::recode(indicator,
                                            "TX_CURR" = "TX_CURR_Previous"),
                  period = period + months(1))

  df <- dplyr::bind_rows(df, tx_curr_prev)

  return(df)

}
