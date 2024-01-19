#' Process monthly enhanced monitoring AHD HIV submission from PEPFAR Mozambique Clinical Partners
#'
#' @param filename Local path to the monthly IP submission
#'
#' @return A tidy dataframe with monthly TB AHD results.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- reshape_em_ahdhiv()}

reshape_em_ahdhiv <- function(filename){
  ip_temp <- mozR::extract_em_meta(filename, type = "ip")
  month_temp <- mozR::extract_em_meta(filename, type = "month")
  data <- readxl::read_xlsx(filename,
                            sheet = "AHD HIV",
                            skip = 8,
                            .name_repair = "unique_quiet")

  df <- data %>%
    dplyr::select(!c(contains("remove"),
                     sisma_nid,
                     No))%>%
    tidyr::pivot_longer('ahd.curr.previous_age__u5_':'ks.diag.tx_pw___',
                        names_to = c("indicator", "pop_type","disaggregate", "age"),
                        names_sep = "_",
                        values_to = "value") %>%
    dplyr::mutate(period = month_temp,
                  partner = ip_temp,
                  indicator = stringr::str_to_upper(stringr::str_replace_all(indicator, "\\.", "_")),
                  age = dplyr::recode(age,
                                      "u5" = "<05",
                                      "5.9" = "05-09",
                                      "10.14" = "10-14",
                                      "15.19" = "15-19",
                                      "o15" = ">15",
                                      "u15" = "<15",
                                      "20p" = "20+"),# new code to correct age
                  pop_type = dplyr::case_when(pop_type == "age" ~ "Age",
                                              pop_type == "pw" ~ "PW",
                                              .default = as.character(pop_type)),
                  disaggregate = dplyr::case_when(disaggregate == "reinit" ~ "reinitiate",
                                                  .default = as.character(disaggregate)),
                  disaggregate = stringr::str_to_title(disaggregate)
    )%>%
    dplyr::rename(snu = snu1)

  return(df)
}

