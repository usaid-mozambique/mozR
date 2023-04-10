#' Extract Metadata from monthly IP sumissions
#' @param filename Local path to the monthly IP submission
#' @param type Meta element for extraction - set to either "ip" or "month"
#' @return Meta values for ip and month
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- extract_em_meta()}

extract_em_meta <- function(filename, type){

  if(type == "ip"){
    df <- read_excel(filename, sheet = "meta", range = "C3:C3", col_names = FALSE)[[1]]
  } else if (type == "month"){
    df <- read_excel(filename, sheet = "meta", range = "C4:C4", col_names = FALSE)[[1]]
  }

  return(df)

}
