#' Process monthly enhanced monitoring DISA submission from PEPFAR Mozambique lab partner APHL
#' @param filename Local path to the monthly DISA submission
#' @return A tidy dataframe with monthly enhanced monitoring DISA results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- reshape_em_disa()}

reshape_em_disa <- function(filename) {

  # disa by age
  xAge <- readxl::read_excel({filename},
                             sheet = "Age & Sex",
                             col_types = c("text", "text", "text", "text", "text",
                                           "text", "text", "text", "text",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric"),
                             skip = 2) %>%
    dplyr::mutate(group = "Age")

  # disa pregnant women
  xPW <- readxl::read_excel({filename},
                            sheet = "S. Viral (M. Gravidas)",
                            col_types = c("text", "text", "text", "text",
                                          "text", "text", "text", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric"),
                            skip = 2) %>%
    dplyr::mutate(group = "PW")

  # disa lactating women
  xLW <- readxl::read_excel({filename},
                            sheet = "S. Viral (M. Lactantes)",
                            col_types = c("text", "text", "text", "text",
                                          "text", "text", "text", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric"),
                            skip = 2) %>%
    dplyr::mutate(group = "LW")

  # disa turn around time
  df_tat <- readxl::read_excel({filename},
                               sheet = "TRL - AVG",
                               col_types = c("text",
                                             "text", "text", "text", "text", "text",
                                             "text", "numeric", "numeric", "numeric",
                                             "numeric", "numeric"),
                               skip = 2) %>%
    dplyr::select(-c(TOTAL))


  # CLEAN VL DATAFRAME ---------------------------------------------------------


  df_vl <- dplyr::bind_rows(xAge, xPW, xLW)

  df_vl <- df_vl %>%
    dplyr::select(-c(`CV < 1000`, `CV > 1000`, TOTAL)) %>%
    dplyr::rename(site_nid = `SISMA ID`,
                  disa_uid = `DISA ID`,
                  snu = PROVINCIA,
                  psnu = DISTRITO,
                  sitename = `U. SANITARIA`,
                  age = Idade,
                  sex = Sexo) %>%
    dplyr::relocate(c(group, disa_uid), .before = sitename) %>%
    tidyr::pivot_longer(`Rotina (<1000)`:`Motivo de Teste n\\u00e3o especificado (>1000)`, names_to = "indicator", values_to = "value") %>%
    dplyr::mutate(motive = dplyr::case_when(grepl("Rotina", indicator) ~ "Routine",
                                            grepl("Fal", indicator) ~ "Theraputic Failure",
                                            grepl("Repetir", indicator) ~ "Post Breastfeeding",
                                            grepl("Motivo de Teste NS", indicator) ~ "Not Specified"),
                  result = dplyr::case_when(grepl("<1000", indicator) ~ "<1000",
                                            grepl(">1000", indicator) ~ ">1000"),
                  tat_step = "temp") %>%
    dplyr::select(-c(indicator)) %>%
    dplyr::mutate(age = dplyr::recode(age, "Idade n\\u00e3o especificada" = "Unknown Age"),
                  age = dplyr::recode(age, "No Age Specified" = "Unknown Age"),
                  age = dplyr::recode(age, "N\\u00e3o especificada" = "Unknown Age"),
                  age = dplyr::recode(age, "N\\u00e3o especificado" = "Unknown Age"),
                  age = dplyr::recode(age, "NS" = "Unknown Age"),
                  age = dplyr::recode(age, "<1" = "<01"),
                  age = tidyr::replace_na(age, "Unknown Age"),
                  sex = dplyr::recode(sex, "UNKNOWN" = "Unknown"),
                  sex = dplyr::recode(sex, "Not Specified" = "Unknown"),
                  sex = dplyr::recode(sex, "N\\u00e3o especificado" = "Unknown"),
                  sex = dplyr::recode(sex, "F" = "Female"),
                  sex = dplyr::recode(sex, "M" = "Male"),
                  sex = tidyr::replace_na(sex, "Unknown")
    ) %>%
    dplyr::filter(value > 0) %>%
    dplyr::mutate(indicator = "VL",
                  period = {period})


  # CLEAN TAT DATAFRAME -----------------------------------------------------


  df_tat <- df_tat %>%
    dplyr::rename(site_nid = `SISMA ID`,
                  disa_uid = `DISA ID`,
                  snu = PROVINCIA,
                  psnu = DISTRITO,
                  sitename = `U. SANITARIA`) %>%
    tidyr::pivot_longer((`COLHEITA \\u00c0 RECEP\\u00c7\\u00e3O`:`AN\\u00c1LISE \\u00c0 VALIDA\\u00c7\\u00e3O`), names_to = "tat_step", values_to = "value") %>%
    dplyr::mutate(tat_step = dplyr::recode(tat_step,
                                           "COLHEITA \\u00c0 RECEP\\u00c7\\u00e3O" = "S1: Collection to Receipt",
                                           "RECEP\\u00c7\\u00e3O AO REGISTO" = "S2: Receipt to Registration",
                                           "REGISTO \\u00c0 AN\\u00c1LISE" = "S3: Registration to Analysis",
                                           "AN\\u00c1LISE \\u00c0 VALIDA\\u00c7\\u00e3O" = "S4: Analysis to Validation"),
                  indicator = "TAT",
                  period = {period})


  disa_vl <- dplyr::bind_rows(df_vl, df_tat)


  # SUBSET VLS DATAFRAME ------------------------------------------------------


  disa_vls <- disa_vl %>%
    dplyr::filter(result == "<1000") %>%
    dplyr::mutate(indicator = "VLS")


  # DATAFRAME COMPILE, GROUP, & PIVOT WIDE ----------------------------------


  disa <- dplyr::bind_rows(disa_vl, disa_vls) %>%
    dplyr::mutate(row = row_number(),
                  tat_step = dplyr::na_if(tat_step, "temp")) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = value, values_fill = NULL) %>%
    dplyr::group_by(period, snu, psnu, sitename, disa_uid, site_nid, age, group, sex, motive, tat_step) %>%
    dplyr::summarise(VL = sum(VL, na.rm = T),
                     VLS = sum(VLS, na.rm = T),
                     TAT = sum(TAT, na.rm = T)) %>%
    dplyr::ungroup()


  # REMOVE OBJECTS ----------------------------------------------------------

  df_vl <- dplyr::bind_rows(xAge, xPW, xLW)

}
