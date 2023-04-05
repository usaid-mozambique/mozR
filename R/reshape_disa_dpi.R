#' Reshape APHL DISA DPI submission
#'
#' @param filename file path and name of APHL DPI submission
#' @param month Month for which DISA DPI is included in submission
#'
#' @return Tidy dataframe with APHL DPI results.  Does not include site metadata.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- reshape_disa_dpi()}

reshape_disa_dpi <- function(filename, month) {

  # tidy aphl disa dpi dataset
  dpi <- readxl::read_excel(filename,
                            col_types = c("text", "text", "text",
                                          "text", "text", "text", "text", "text",
                                          "numeric", "numeric", "text", "text",
                                          "text", "text", "text"),
                            skip = 5) %>%

    dplyr::select(!c(site_nid,
                     datim_uid,
                     sitetype)) %>%

    tidyr::pivot_longer(cols = starts_with("dpi_"),
                        names_to = c("indicator", "disaggregate", "result"),
                        names_sep = "_",
                        values_to = "value",
                        values_transform = list(value = as.numeric)) %>%

    dplyr::filter(!result == "total",
                  value > 0) %>%

    dplyr::mutate(period = month,
                  indicator = "DISA_DPI",
                  result = dplyr::case_when(result == "positive" ~ "Positive",
                                            result == "negative" ~ "Negative",
                                            result == "indet" ~ "Indet.",
                                            TRUE ~ result),
                  disaggregate = dplyr::case_when(disaggregate == "conventional" ~ "Conventional",
                                                  disaggregate == "mpima" ~ "MPIMA",
                                                  TRUE ~ disaggregate),
                  sex = dplyr::case_when(sex == "F" ~ "Female",
                                         sex == "M" ~ "Male",
                                         stringr::str_detect(sex, "speci") ~ "Unknown",
                                         TRUE ~ sex),
                  tat_step = NA_character_) %>%

    dplyr::relocate(indicator, .after = sitename) %>%
    dplyr::relocate(period, .before = everything())

  # create dataframe with positive dpi results
  dpi_pos <- dpi %>%
    dplyr::filter(result == "Positive") %>%
    dplyr::mutate(indicator = "DISA_DPI_POS")


  df_tat <- readxl::read_excel(filename,
                               sheet = "TRL-AVG", col_types = c("text",
                                                                "text", "text", "text", "text", "text",
                                                                "text", "numeric", "numeric", "numeric",
                                                                "numeric", "numeric", "numeric"),
                               skip = 4) %>%

    tidyr::pivot_longer(cols = starts_with("dpi"),
                        names_to = c("indicator", "disaggregate", "result", "remove", "tat_step"),
                        names_sep = "_",
                        values_to = "value",
                        values_transform = list(value = as.numeric)) %>%

    dplyr::select(!c(site_nid,
                     datim_uid,
                     sitetype,
                     remove)) %>%

    dplyr::mutate(period = month,
                  indicator = "TAT",
                  sex = NA_character_,
                  tat_step = dplyr::case_when(str_detect(tat_step, "s1.") ~ "S1: Collection to Disa-Link",
                                              str_detect(tat_step, "s2.") ~ "S2: Disa-Link to Lab",
                                              str_detect(tat_step, "s3.") ~ "S3: Lab to Register",
                                              str_detect(tat_step, "s4.") ~ "S4: Register to Analysis",
                                              str_detect(tat_step, "s5.") ~ "S5: Analysis to Validation",
                                              TRUE ~ tat_step)) %>%

    dplyr::filter(!tat_step == "total")

  # row bind all dpi dataframe to positive subset dataframe
  df <- dplyr::bind_rows(dpi, dpi_pos, df_tat) %>%
    dplyr::relocate(tat_step, .before = value)

  return(df)

}
