#' A general function for cleaning a CSV export from SISMA
#'
#' @param file CSV input pile from SISMA
#'
#' @return A long format of original CSV export from SISMA
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- clean_sisma_csv()}

clean_sisma_csv <- function(file) {

  df <- readr::read_csv(file) %>%

    dplyr::select(!c(periodname,
                     periodid,
                     perioddescription,
                     organisationunitcode,
                     organisationunitdescription,
                     orgunitlevel1,
                     organisationunitname)) %>%

    tidyr::pivot_longer(!c(periodcode,
                           orgunitlevel2,
                           orgunitlevel3,
                           orgunitlevel4,
                           organisationunitid),
                        names_to = "indicator",
                        values_to = "value",
                        values_transform = list(value = as.numeric)) %>%

    dplyr::filter(!is.na(value)) %>%

    dplyr::mutate(periodcode = paste0(periodcode, "01"),
                  periodcode = ymd(periodcode),
                  across(c(orgunitlevel2, orgunitlevel3), ~ str_to_title(.))) %>%

    dplyr::select(period = periodcode,
                  snu = orgunitlevel2,
                  psnu = orgunitlevel3,
                  sitename = orgunitlevel4,
                  sisma_uid = organisationunitid,
                  indicator,
                  value)

  return(df)

}
