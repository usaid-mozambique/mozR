#' Process monthly enhanced monitoring TPT submission from PEPFAR Mozambique Clinical Partners
#' @param filename Local path to the monthly IP submission
#' @return A tidy dataframe with monthly enhanced monitoring TPT results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- reshape_em_tpt()}


reshape_em_tpt <- function(filename){

  ip_temp <- extract_em_meta(filename, type = "ip")
  month_temp <- extract_em_meta(filename, type = "month")

  df <- read_excel(filename,
                   sheet = "TPT Completion",
                   skip = 7,
                   .name_repair = "unique_quiet") %>%

    dplyr::filter(Partner == ip_temp) %>%

    dplyr::select(partner = Partner,
                  snu = Province,
                  psnu = District,
                  sitename = `Health Facility`,
                  datim_uid = DATIM_code,
                  TX_CURR,
                  TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init) %>%

    dplyr::mutate(TPT_candidates = TX_CURR - (TX_CURR_TPT_Com + TX_CURR_W_TPT_last7Mo) - (TX_CURR_TB_tto + TX_CURR_TPT_Not_Comp_POS_Screen),
                  TPT_ineligible = TX_CURR_TB_tto + TX_CURR_TPT_Not_Comp_POS_Screen,
                  TPT_active_complete = TX_CURR_W_TPT_last7Mo + TX_CURR_TPT_Com) %>%

    tidyr::pivot_longer(TX_CURR:TPT_active_complete, names_to = "attribute", values_to = "value") %>%

    dplyr::mutate(indicator = attribute) %>%

    dplyr::mutate(indicator = recode(indicator,
                                     "TX_CURR_W_TPT_last7Mo" = "Actively on TPT", # use to create new indicator
                                     "TX_CURR_TB_tto" = "Recent Active TB TX",
                                     "TX_CURR_TPT_Not_Comp_POS_Screen" = "Recent Pos TB Screen",
                                     "TX_CURR_TPT_Com" = "TPT Completed",  # use to create new indicator
                                     "TPT_candidates" = "TPT Candidates",
                                     "TPT_ineligible" = "TPT Ineligible",
                                     "TX_CURR_TPT_Not_Comp" = "TPT Not Comp",
                                     "TPT_active_complete" = "TPT Completed/Active"),
                  period = month_temp,
    ) %>%
    dplyr::filter(!indicator %in% c("TX_CURR_Eleg_TPT_Init", "TX_CURR_Eleg_TPT_Comp")) %>%
    dplyr::select(partner,
                  snu,
                  psnu,
                  sitename,
                  datim_uid,
                  period,
                  indicator,
                  attribute,
                  value)

  return(df)

}
