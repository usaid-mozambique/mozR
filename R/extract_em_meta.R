#' Extract Partner and Period Metadata from monthly EM Excel submission
#'
#' @param filename Path to EM Excel submission
#' @param type Metadata to return.  Set to either "ip" or "month"
#' @return values for Partner and Month
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- extract_em_meta()}

extract_em_meta <- function(filename, type) {

  if(type == "ip"){
    df <- read_excel(filename, sheet = "meta", range = "C3:C3", col_names = FALSE)[[1]]
  } else if (type == "month"){
    df <- read_excel(filename, sheet = "meta", range = "C4:C4", col_names = FALSE)[[1]]
  }

  return(df)

}
