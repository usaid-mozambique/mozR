
#' Pull the supplychain metadata
#'
#' @param sheetname name of the google sheet
#'
#' @return metadata
#' @export
#'
#' @examples
#'   \dontrun{
#'    pull_supplychain_metadata(sheetname = "product_metadata")
#' }
pull_supplychain_metadata <- function(sheetname = "product_metadata") {

  #save google id
  path_SC_metadata <- googlesheets4::as_sheets_id("1sECcZA_kB_fh-9EuVkfUSz17BEzasNSP9lW-5YB4ByU")
  sc_metadata <- googlesheets4::read_sheet(path_SC_metadata, sheet = sheetname)

  return(sc_metadata)

}
