#' Reshape APHL DISA DPI submission
#'
#' @param filename file path and name of APHL DPI submission
#'
#' @return Tidy dataframe with APHL DPI results.  Does not include site metadata.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- reshape_disa_dpi()}

reshape_disa_dpi <- function(filename) {

  # tidy aphl disa dpi dataset
  dpi <- readxl::read_excel(filename,
                            col_types = c("text", "text", "text",
                                          "text", "text", "text", "text", "text",
                                          "numeric", "numeric", "text", "text",
                                          "text", "text", "text"),
                            skip = 5) %>%

    dplyr::select(!c(sisma_nid,
                     datim_uid,
                     ajuda_site)) %>%

    tidyr::pivot_longer(cols = starts_with(c("conventional", "mpima")),
                        names_to = c("type", "result"),
                        names_sep = "_",
                        values_to = "value",
                        values_transform = list(value = as.numeric)) %>%

    dplyr::filter(!result == "total",
                  value > 0) %>%

    dplyr::mutate(period = month,
                  indicator = "DISA_DPI",
                  result = dplyr::case_when(result == "pos" ~ "Positive",
                                            result == "neg" ~ "Negative",
                                            result == "indet" ~ "Indet."),
                  type = dplyr::case_when(type == "conventional" ~ "Conventional",
                                          type == "mpima" ~ "MPIMA"),
                  sex = dplyr::case_when(sex == "F" ~ "Female",
                                         sex == "M" ~ "Male")) %>%

    dplyr::relocate(indicator, .after = sitename) %>%
    dplyr::relocate(period, .before = everything())

  # create dataframe with positive dpi results
  dpi_pos <- dpi %>%
    dplyr::filter(result == "Positive") %>%
    dplyr::mutate(indicator = "DISA_DPI_POS")

  # row bind all dpi dataframe to positive subset dataframe
  df <- dplyr::bind_rows(dpi, dpi_pos)

  return(df)

}
