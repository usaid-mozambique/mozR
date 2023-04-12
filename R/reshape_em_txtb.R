#' Process monthly enhanced monitoring TXTB submission from PEPFAR Mozambique Clinical Partners
#' @param filename Local path to the monthly IP submission
#' @return A tidy dataframe with monthly enhanced monitoring TXTB results
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- reshape_em_txtb()}

reshape_em_txtb <- function(filename){

  ip_temp <- extract_em_meta(filename, type = "ip")
  month_temp <- extract_em_meta(filename, type = "month")

  df <- read_excel(filename,
                   sheet = "TX_TB",
                   skip = 7) %>%
    dplyr::filter(partner == ip_temp) %>%

    dplyr::select(!c(contains(c("remove", "tot")),
                     sisma_nid,
                     No)) %>%

    dplyr::rename(snu = snu1) %>%

    tidyr::pivot_longer('TX.CURR_newART_Male_<15':'TX.TB.CURR.N_alreadyART_Female_Unk',
                        names_to = c("indicator", "disaggregate", "sex", "age"),
                        names_sep = "_",
                        values_to = "value") %>%

    dplyr::mutate(period = month_temp,
                  indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                  age = dplyr::recode(age,
                                      "Unk" = "Unknown Age"), # new code to correct age
                  disaggregate = dplyr::recode(disaggregate,
                                               "newART" = "New on ART",
                                               "alreadyART" = "Already on ART"))

  return(df)

}

