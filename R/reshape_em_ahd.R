#' Process monthly enhanced monitoring TB AHD submission from PEPFAR Mozambique Clinical Partners
#'
#' @param filename Local path to the monthly IP submission
#'
#' @return A tidy dataframe with monthly TB AHD results.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- reshape_em_ahd()}

reshape_em_ahd <- function(filename){
  ip_temp <- mozR::extract_em_meta(filename, type = "ip")
  month_temp <- mozR::extract_em_meta(filename, type = "month")

  df <- readxl::read_excel(filename,
                           sheet = "AHD TB",
                           skip = 5,
                           .name_repair = "unique_quiet") %>%

    dplyr::select(!c(contains("remove")),
                  sisma_nid) %>%
    tidyr::pivot_longer('CD4.Elig__Male_u1':'TB.TX_GradeNR',
                        names_to = c("indicator", "disaggregate", "sex", "age"),
                        names_sep = "_",
                        values_to = "value") %>%
    dplyr::mutate(period = month_temp,
                  partner = ip_temp,
                  indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                  age = dplyr::recode(age,
                                      "Unk" = "Unknown Age",
                                      "u1" = "<01",
                                      "1.4" = "01-04",
                                      "5.9" = "05-09",
                                      "10.14" = "10-14",
                                      "15.19" = "15-19",
                                      "20" = "20+"),# new code to correct age
                  disaggregate = case_when(disaggregate %in% c("LAM", "woLAM", "Grade1",
                                                               "Grade2", "Grade3", "Grade4",
                                                               "GradeNR") ~ disaggregate,
                                           TRUE ~ NA)
    ) %>%
    dplyr::rename(snu = snu1)

  return(df)
}
