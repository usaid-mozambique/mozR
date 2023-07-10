#' Process monthly enhanced monitoring DSD submission from PEPFAR Mozambique Clinical Partners
#' @param filename Local path to the monthly IP submission
#'
#' @return A tidy dataframe with monthly enhanced monitoring DSD results
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- reshape_em_dsd()}

reshape_em_dsd <- function(filename) {

  ip_temp <- extract_em_meta(filename, type = "ip")
  month_temp <- extract_em_meta(filename, type = "month")

  df <- readxl::read_excel(filename, # function argument
                           sheet = "MDS",
                           skip = 8,
                           .name_repair = "unique_quiet") %>%

    dplyr::select(!c(contains("remove"))) %>%

    tidyr::pivot_longer(TX.ACTIVE.ELIGIBILITY_ELI_ADULT_15p:DSD.ONE.ALL__LW_15p,
                        names_to = c("indicator", "dsd_eligibility", "pop_type", "age"),
                        names_sep = "_",
                        values_to = "value") %>%

    dplyr::filter(Partner == ip_temp,
                  !stringr::str_detect(indicator, "remove")) %>%

    dplyr::mutate(period = month_temp,
                  indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                  age = stringr::str_replace_all(age, "\\.", "-"),
                  age = dplyr::recode(age,
                                      `2u`    = "<02",
                                      `2-4`   = "02-04",
                                      `5-9`   = "05-09",
                                      `15p`   = "15+",
                                      Unknown = "Unknown Age"),
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

  return(df)

}
