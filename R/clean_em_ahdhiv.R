#' Join AJUDA site metadata and clean processed monthly enhanced monitoring AHD HIV dataframe
#'
#' @param df Processed monthly enhanced monitoring AHD HIV dataframe
#' @param ajuda_site_map Ajuda_site_map data
#'
#' @return Final AHD HIV dataframe used in analytic dashboards
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- clean_em_ahdhiv()}

clean_em_ahdhiv <- function(df, ajuda_site_map){
  df_cleaned <- df %>%
    dplyr::select(!c(partner, snu, psnu,
                     sitename)) %>%
    dplyr::left_join(ajuda_site_map, by = "datim_uid") %>%
    dplyr::select(datim_uid,
                  sisma_uid,
                  sisma_uid_datim_map,
                  site_nid,
                  period,
                  partner = partner_pepfar_clinical,
                  snu,
                  psnu,
                  sitename,
                  grm_sernap,
                  cop_entry,
                  ends_with("tude"),
                  starts_with("program_"),
                  starts_with("his_"),
                  indicator,
                  pop_type,
                  disaggregate,
                  age,
                  value
    )
  return(df_cleaned)
}
