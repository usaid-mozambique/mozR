#' Import AJUDA site map
#'
#' @return A dataframe of 640 rows for all AJUDA site metadata
#' @export
#'
#' @examples
#'  \dontrun{
#'  site_map <- pull_ajuda_sitemap()
#' }
#'


pull_ajuda_sitemap <- function() {

#save google id
path_ajuda_site_map <- googlesheets4::as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U")

ajuda_site_map <- googlesheets4::read_sheet(path_ajuda_site_map, sheet = "list_ajuda")

return(ajuda_site_map)

}
