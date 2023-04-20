#' Process monthly enhanced monitoring IMER submission from PEPFAR Mozambique Clinical Partners
#' @param filename Local path to the monthly IP submission
#' @param type Metadata type - ip or month
#' @return Metadata values
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- extract_em_meta()}

extract_em_meta <- function(filename, type){

  if(type == "ip"){
    df <- read_excel(filename, sheet = "meta", range = "C3:C3", col_names = FALSE, .name_repair = "unique_quiet")[[1]]
  } else if (type == "month"){
    df <- read_excel(filename, sheet = "meta", range = "C4:C4", col_names = FALSE, .name_repair = "unique_quiet")[[1]]
  }

  return(df)

}
