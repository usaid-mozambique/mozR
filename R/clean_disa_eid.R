#' Clean historic DISA DPI dataframe loaded from historic monthly data
#'
#' @param df Dataframe of historic monthly DISA DPI data before cleaning and join of metadata
#'
#' @return Final cleaned historic DISA DPI dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  df <- clean_disa_eid()}

clean_disa_eid <- function(df){

  df <- df %>%
    tidyr::drop_na(datim_uid) %>%
    dplyr::select(!c(snu, psnu, sitename)) %>%

    dplyr::left_join(datim_orgsuids, by = "datim_uid") %>%
    dplyr::left_join(ajuda_site_map, by = "datim_uid") %>%
    dplyr::left_join(psnuuid_map, by = "psnu") %>%

    dplyr::mutate(
      partner = tidyr::replace_na(partner, "MISAU"),
      support_type = dplyr::case_when(
        partner == "MISAU" ~ "Sustainability",
        TRUE ~ as.character("AJUDA")),

      agency = dplyr::case_when(
        partner == "MISAU" ~ "MISAU",
        partner == "JHPIEGO-DoD" ~ "DOD",
        partner == "ECHO" ~ "USAID",
        TRUE ~ as.character("HHS/CDC")),

      psnuuid = dplyr::case_when(
        partner == "JHPIEGO-DoD" ~ "siMZUtd2cJW",
        TRUE ~ psnuuid),

      dplyr::across(c(snu, psnu, sitename), ~ dplyr::case_when(partner == "JHPIEGO-DoD" ~ "_Military",
                                                               TRUE ~ .))) %>%

    dplyr::select(period,
                  sisma_uid,
                  datim_uid,
                  disa_uid,
                  site_nid,
                  ajuda,
                  starts_with("his_"),
                  snu,
                  psnu,
                  psnuuid,
                  sitename,
                  support_type,
                  partner,
                  agency,
                  sex,
                  disaggregate,
                  result,
                  tat_step,
                  indicator,
                  value)

  return(df)

}
