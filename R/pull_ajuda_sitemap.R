#' Retrieve PEPFAR Mozambique AJUDA Site Map from Google Sheets
#'
#' @return A dataframe containing metadata for all PEPFAR Mozambique AJUDA sites
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- pull_ajuda_sitemap()
#' }
#'


pull_ajuda_sitemap <- function() {

#save google id
path_ajuda_site_map <- googlesheets4::as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U")

ajuda_site_map <- googlesheets4::read_sheet(path_ajuda_site_map, sheet = "list_ajuda")

return(ajuda_site_map)

}
