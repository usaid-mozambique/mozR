#' Process quarterly MER supplemental Key Population report
#'
#' @param filename File containing quarterly supplemental MER results
#'
#' @return A tidy dataframe containing Key Population results - TX_PLVS, TX_RET, TX_CURR
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- reshape_sup_kp()}

reshape_sup_kp <- function(filename){

  ip_temp <- extract_em_meta(filename, type = "ip")

  month_temp <- extract_em_meta(filename, type = "month") %>%
    stringr::str_replace("FY", "20")

  df <- readxl::read_excel(filename, sheet = "POP CHAVES - Trimestre", skip = 9) %>%
    dplyr::select(-c(No, SISMA_code, Data, Column1)) %>%
    dplyr::rename(partner =  Partner,
                  snu = Province,
                  psnu = District,
                  sitename = `Health Facility`,
                  datim_uid = DATIM_code) %>%

    tidyr::pivot_longer(TX_New_KP_total:`TX_PVLS_Num_6meses_REC&MTS_25+`,
                        names_to = "temp",
                        values_to = "value") %>%

    dplyr::filter(partner == ip_temp,
                  value > 0) %>%

    dplyr::mutate(indicator = dplyr::case_when(
      stringr::str_detect(temp, "TX_CURR_6meses")  ~ "TX_CURR_6MO",
      stringr::str_detect(temp, "TX_CURR_meses")  ~ "TX_CURR_6MO",  # CORRECT IN REPORTING TEMPLATE?
      stringr::str_detect(temp, "TX_New_6meses")  ~ "TX_NEW_6MO",
      stringr::str_detect(temp, "TX_CURR_12meses")  ~ "TX_RET_N_12MO",
      stringr::str_detect(temp, "TX_New_12meses")  ~ "TX_RET_D_12MO",
      stringr::str_detect(temp, "TX_New")  ~ "TX_NEW",  # CORRECT IN REPORTING TEMPLATE?
      stringr::str_detect(temp, "TX_CURR")  ~ "TX_CURR",
      stringr::str_detect(temp, "TX_PVLS_Den_6meses")  ~ "TX_PVLS_D_6MO",
      stringr::str_detect(temp, "TX_PVLS_Den_meses")  ~ "TX_PVLS_D_6MO",   # CORRECT IN REPORTING TEMPLATE?
      stringr::str_detect(temp, "TX_PVLS_Num_6meses")  ~ "TX_PVLS_N_6MO",
      stringr::str_detect(temp, "TX_PVLS_Num")  ~ "TX_PVLS_N",
      stringr::str_detect(temp, "TX_PVLS_Den")  ~ "TX_PVLS_D"
    ),
    keypop = dplyr::case_when(stringr::str_detect(temp, "otal")  ~ "All (KP & non-KP)",
                              stringr::str_detect(temp, "_PID_")  ~ "PID",
                              stringr::str_detect(temp, "_HSH_")  ~ "HSH",
                              stringr::str_detect(temp, "_MTS_")  ~ "MTS",
                              stringr::str_detect(temp, "_Rec_")  ~ "REC",  # CORRECT IN REPORTING TEMPLATE?
                              stringr::str_detect(temp, "_PID&HSH_")  ~ "PID/HSH",
                              stringr::str_detect(temp, "_PID&MST_")  ~ "PID/MTS",  # CORRECT IN REPORTING TEMPLATE?
                              stringr::str_detect(temp, "_PID&Rec_")  ~ "PID/REC",
                              stringr::str_detect(temp, "_HSH&PID_")  ~ "HSH/PID",
                              stringr::str_detect(temp, "_HSH&Rec_")  ~ "HSH/REC",
                              stringr::str_detect(temp, "_MTS&PID_")  ~ "MTS/PID",
                              stringr::str_detect(temp, "_MTS&Rec_")  ~ "MTS/REC",
                              stringr::str_detect(temp, "_REC&PID_")  ~ "REC/PID",
                              stringr::str_detect(temp, "_REC&HSH_")  ~ "REC/HSH",
                              stringr::str_detect(temp, "_REC&MTS_")  ~ "REC/MTS",
                              stringr::str_detect(temp, "_MST")  ~ "MTS",  # CORRECT IN REPORTING TEMPLATE?
                              stringr::str_detect(temp, "_REC_")  ~ "REC"),
    age = dplyr::case_when(stringr::str_detect(temp, "15_19")  ~ "15-19",
                           stringr::str_detect(temp, "20_24")  ~ "20-24",
                           stringr::str_detect(temp, "25+")  ~ "25+")
    ) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = value) %>%

    dplyr::mutate(date = month_temp,
                  pop_type = case_when(keypop == "All (KP & non-KP)" ~ "General",
                                       TRUE ~ "Key Pop"),
                  keypop = case_when(keypop == "All (KP & non-KP)" ~ "",
                                     TRUE ~ keypop)) %>%
    dplyr::select(snu,
                  psnu,
                  sitename,
                  datim_uid,
                  pop_type,
                  keypop,
                  age,
                  period = date,
                  starts_with("TX_"))

  return(df)

}
