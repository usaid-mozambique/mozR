#' Function that will clean sisma csv export and parse it into a tidy datafrom
#'
#' @param file Path of sisma csv input
#' @param type Type of sisma csv export (CPN, ATS-R, etc.)
#'
#' @return A tidy data frame of sisma program results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- process_sisma_csv()}

process_sisma_csv <- function(file, type){

  df <- clean_sisma_csv(file) %>%
    parse_sisma_csv(type)

  return(df)

}
