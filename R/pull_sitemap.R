#' Retreive site metadata
#'
#'  `pull_sitemap` utilizes stored user credentials to access
#'  a GoogleSheet where Mozambique health facility metadata is
#'  contained.  This includes site GIS coordinates, USG program
#'  support definition, mapping of existing HIS, and unique system
#'  identifiers
#'
#' @param sheetname Sheet name from the Mozambique AJUDA Site Map
#' @return A dataframe containing site metadata
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  #fetch metadata for current PEPFAR AJUDA sites in Mozambique
#'  df <- pull_sitemap()
#'
#'  #fetch DISA uid's mapped to Datim/Sisma uid
#'  df <- pull_sitemap(sheet = "map_disa")
#' }


pull_sitemap <- function(sheetname = "list_ajuda") {

  #save google id
  path_site_map <- googlesheets4::as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U")

  site_map <- googlesheets4::read_sheet(path_site_map, sheet = sheetname)

  return(site_map)

}

