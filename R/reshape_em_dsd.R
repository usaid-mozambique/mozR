#' Process monthly enhanced monitoring DSD submission from PEPFAR Mozambique Clinical Partners
#'
#' @param df Local path to the monthly IP submission
#' @param ip IP whose submission the file pertains to
#'
#' @return A tidy dataframe with monthly enhanced monitoring DSD results
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- reshape_em_dsd()}

reshape_em_dsd <- function(df, ip) {

  df <- readxl::read_excel(df, # function argument
                           sheet = "MDS",
                           skip = 8) %>%
    dplyr::select(!c(No, SISMA_code, Period)) %>%
    tidyr::pivot_longer(remove.1:DSD.AHD__LW_15p,
                        names_to = c("indicator", "dsd_eligibility", "pop_type", "age"),
                        names_sep = "_",
                        values_to = "value") %>%
    dplyr::filter(Partner == ip, # function argument
                  !str_detect(indicator, "remove")) %>%
    dplyr::mutate(period = as.Date(month, "%Y-%m-%d"),
                  indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                  age = stringr::str_replace_all(age, "\\.", "-"),
                  age = dplyr::case_when(age == "15p" ~ "15+",
                                         age == "2u" ~ "<2",
                                         TRUE ~ age),
                  dsd_eligibility = dplyr::recode(dsd_eligibility,
                                                  ELI = "Eligible",
                                                  NEL = "Non-Eligible",
                                                  TOTAL = NA_character_),
                  pop_type = dplyr::recode(pop_type,
                                           ADULT = "Adult",
                                           PED = "Pediatric")) %>%
    dplyr::select(partner = Partner,
                  snu = Province,
                  psnu = District,
                  sitename = `Health Facility`,
                  datim_uid = DATIM_code,
                  period,
                  indicator,
                  dsd_eligibility,
                  pop_type,
                  age,
                  value)

}
