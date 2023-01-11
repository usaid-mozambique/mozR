#' Process monthly enhanced monitoring TXTB submission from PEPFAR Mozambique Clinical Partners
#' @param filename Local path to the monthly IP submission
#' @param ip IP whose submission the file pertains to
#' @return A tidy dataframe with monthly enhanced monitoring TXTB results
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- reshape_em_txtb()}

reshape_em_txtb <- function(filename, ip){

  df <- readxl::read_excel(filename,
                           sheet = "TX_TB",
                           col_types = c("text", "text", "text",
                                         "text", "text", "text", "text",
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
                                         "numeric"), skip = 7) %>%
    dplyr::filter(partner == ip) %>%
    dplyr::select(!c(contains(c("remove", "tot")))) %>%
    tidyr::pivot_longer('TX.CURR_newART_Male_<15':'TX.TB.CURR.N_alreadyART_Female_Unk',
                        names_to = c("indicator", "disaggregate", "sex", "age"),
                        names_sep = "_",
                        values_to = "value") %>%
    dplyr::mutate(period = as.Date(month, "%Y-%m-%d"),
                  indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                  age = dplyr::recode(age, "Unk" = "Unknown"),
                  disaggregate = dplyr::recode(disaggregate,
                                               "newART" = "New on ART",
                                               "alreadyART" = "Already on ART")) %>%
    tidyr::pivot_wider(names_from =  indicator, values_from = value)

}

