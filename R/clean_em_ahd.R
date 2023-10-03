#' Join AJUDA site metadata and clean processed monthly enhanced monitoring TB AHD dataframe
#' @param df Processed monthly enhanced monitoring TB AHD dataframe
#' @return Final TB AHD dataframe used in analytic dashboards
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- clean_em_ahd()}


clean_em_ahd <- function(df){

  df_cleaned <- df %>%
    dplyr::select(!c(partner, snu1, psnu,
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
                  disaggregate,
                  sex,
                  age,
                  value)


  return(df_cleaned)

}
