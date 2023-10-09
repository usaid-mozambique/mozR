#' Process monthly enhanced monitoring PREP submission from PEPFAR Mozambique Clinical Partners
#' @param filename Local path to the monthly IP submission
#' @return A tidy dataframe with monthly enhanced monitoring PREP results
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- reshape_em_prep()}

reshape_em_prep <- function(filename){

  ip_temp <- extract_em_meta(filename, type = "ip")   # Function argument
  month_temp <- extract_em_meta(filename, type = "month")   # Function argument

  df <- read_excel(filename,   # Function argument
                   sheet = "Resumo Mensal de PrEP",
                   skip = 7,
                   .name_repair = "unique_quiet") %>%

    dplyr::filter(Partner == ip_temp) %>%

    dplyr::select(c(No:PrEP.CT_TP_Outro_Total)) %>%

    dplyr::select(!c(Elegible.to.PrEP_All_Total_Total,
                     Elegible.to.PrEP_Casos.Especiais_Male_Total,
                     Elegible.to.PrEP_Casos.Especiais_Female_Total,
                     Elegible.to.PrEP_All_PW_Total,
                     Elegible.to.PrEP_All_LW_Total,
                     PrEP.NEW_All_Total_Total,
                     PrEP.NEW_Casos.Especiais_Male_Total,
                     PrEP.NEW_Casos.Especiais_Female_Total,
                     PrEP.NEW_All_PW_Total,
                     PrEP.NEW_All_LW_Total,
                     PrEP.New.Who.RTT_All_Total_Total,
                     PrEP.New.Who.RTT_Casos.Especiais_Male_Total,
                     PrEP.New.Who.RTT_Casos.Especiais_Female_Total,
                     PrEP.New.Who.RTT_All_PW_Total,
                     PrEP.New.Who.RTT_All_LW_Total,
                     PrEP.CT_All_Total_Total,
                     PrEP.CT_Casos.Especiais_Male_Total,
                     PrEP.CT_Casos.Especiais_Female_Total,
                     PrEP.CT_All_PW_Total,
                     PrEP.CT_All_LW_Total)) %>%

  # Removed below vars from Sept '23
  # PrEP.CT.3months_All_Total_Total,
  # PrEP.CT.3months_Casos.Especiais_Male_Total,
  # PrEP.CT.3months_Casos.Especiais_Female_Total,
  # PrEP.CT.3months_All_PW_Total,
  # PrEP.CT.3months_All_LW_Total

    tidyr::pivot_longer('Elegible.to.PrEP_Casos.Especiais_Male_10.14':'PrEP.CT_TP_Outro_Total',
                        names_to = c("indicator", "pop_type", "disaggregate", "age"),
                        names_sep = "_",
                        values_to = "value") %>%

    dplyr::mutate(period = month_temp,
                  indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                  indicator = stringr::str_replace_all(indicator, "Elegible_to_PrEP", "PrEP_Eligible"),
                  indicator = stringr::str_replace_all(indicator, "PrEP_New_Who_RTT", "PrEP_NEW_RTT"),
                  age = stringr::str_replace_all(age, "\\.", "-"),
                  age = stringr::str_replace_all(age, "Total", "Unknown Age"), # new code, changed from Unknown to Unknown Age
                  sex = dplyr::case_when(
                    disaggregate == "Female" ~ "Female",
                    disaggregate == "Male" ~ "Male",
                    TRUE ~ as.character("Unknown")),
                  pop_type = dplyr::recode(pop_type,
                                           "Casos.Especiais" = "Special Cases",
                                           "TP.AtRisk" = "TP at Risk",
                                           "All" = "PLW"),
                  disaggregate = dplyr::recode(disaggregate,
                                               "Long.Distance.driver" = "Long Distance Drivers",
                                               "military" = "Military",
                                               "miner" = "Miners",
                                               "People.who.Injected.Drugs" = "PWID",
                                               "Sero.Discordante.Couples" = "Sero-Discordante Couples",
                                               "Sex.workers" = "Sex Workers",
                                               "TG" = "Transgender")) %>%

    dplyr::select(partner = Partner,
                  snu = Province,
                  psnu = District,
                  sitename = `Health Facility`,
                  datim_uid = `Datim Code`,
                  period,
                  indicator,
                  pop_type,
                  disaggregate,
                  sex,
                  age,
                  value
    )

  return(df)

}
