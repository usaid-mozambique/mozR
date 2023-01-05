#' Retrieve PEPFAR Mozambique AJUDA Site Map from Google Sheets
#'
#' @param sheetname Sheet name from the Mozambique AJUDA Site Map
#' @return A dataframe containing metadata for all PEPFAR Mozambique AJUDA sites
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- pull_sitemap()
#' }
#'


pull_sitemap <- function(sheetname = "list_ajuda") {

  #save google id
  path_site_map <- googlesheets4::as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U")

  site_map <- googlesheets4::read_sheet(path_site_map, sheet = sheetname)

  return(site_map)

}
