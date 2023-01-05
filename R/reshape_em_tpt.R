#' Process monthly enhanced monitoring TPT submission from PEPFAR Mozambique Clinical Partners
#' @param filename Local path to the monthly IP submission
#' @param ip IP whose submission the file pertains to
#' @return A tidy dataframe with monthly enhanced monitoring TPT results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- reshape_em_tpt()}

reshape_em_tpt <- function(filename, ip){

  df <- readxl::read_excel(filename, sheet = "TPT Completion",
                           range = "A8:P650",
                           col_types = c("numeric",
                                         "text", "text", "text", "text", "text",
                                         "numeric", "text", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric"),
                           skip = 7) %>%
    dplyr::select(No,
                  Partner,
                  Province,
                  District,
                  `Health Facility`,
                  DATIM_code,
                  SISMA_code,
                  Period,
                  TX_CURR,
                  TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init) %>%
    dplyr::filter(Partner == ip)

  return(df)

}
