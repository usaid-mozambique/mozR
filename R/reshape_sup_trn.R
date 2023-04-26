#' Process quarterly MER supplemental RTT and Transfer-in results
#'
#' @param filename File containing quarterly supplemental MER results
#'
#' @return A tidy dataframe containing supplemental RTT and TRNIF results
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- reshape_sup_trn()}

reshape_sup_trn <- function(filename){


  ip_temp <- extract_em_meta(filename, type = "ip")

  month_temp <- extract_em_meta(filename, type = "month") %>%
    stringr::str_replace("FY", "20")

  df <- read_excel(filename,
                   sheet = "SupplementalMER",
                   skip = 7) %>%

    dplyr::select(partner = mech_name,
                  snu, psnu, sitename,
                  datim_uid = DATIM_code,
                  `TX_RTT Total`:TRF_IN_Female_Unk) %>%

    dplyr::select(!c(contains("emove"), contains("heck"))) %>%

    dplyr::filter(partner == ip_temp) %>%

    dplyr::mutate(dplyr::across(`TX_RTT Total`:TRF_IN_Female_Unk, as.numeric)) %>%

    tidyr::pivot_longer(!c(partner, snu, psnu, sitename, datim_uid),
                        names_to = "temp",
                        values_to = "value") %>%

    dplyr::mutate(indicator = dplyr::case_when(str_detect(temp, "TX_RTT") ~ "TX_RTT",
                                               TRUE ~ "TX_TRNIF"),

                  otherdisaggregate = dplyr::case_when(str_detect(temp, "<12_months") ~ "<12 months",
                                                       TRUE ~ "12+ months"),
                  sex = dplyr::case_when(stringr::str_detect(temp, "Female") ~ "Female",
                                         stringr::str_detect(temp, "Male") ~ "Male",
                                         TRUE ~ "Unknown"),
                  ageasentered = dplyr::case_when(stringr::str_detect(temp, "<01") ~ "<01",
                                                  stringr::str_detect(temp, "1_4") ~ "01-04",
                                                  stringr::str_detect(temp, "5_9") ~ "05-09",
                                                  stringr:: str_detect(temp, "10_14") ~ "10-14",
                                                  stringr::str_detect(temp, "15_19") ~ "15-19",
                                                  stringr::str_detect(temp, "20_24") ~ "20-24",
                                                  stringr::str_detect(temp, "25_29") ~ "25-29",
                                                  stringr::str_detect(temp, "30_34") ~ "30-34",
                                                  stringr::str_detect(temp, "35_39") ~ "35-39",
                                                  stringr::str_detect(temp, "40_44") ~ "40-44",
                                                  stringr::str_detect(temp, "45_49") ~ "45-49",
                                                  stringr::str_detect(temp, "50_54") ~ "50-54",
                                                  stringr::str_detect(temp, "55_59") ~ "55-59",
                                                  stringr::str_detect(temp, "60_64") ~ "60-64",
                                                  stringr::str_detect(temp, ">=65") ~ "65+",
                                                  TRUE ~ "Unknown"),
                  period = period,
                  partner = ip_temp,
                  row_n = dplyr::row_number()) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = value) %>%
    dplyr::select(!c(temp,
                     row_n))


  return(df)

}
