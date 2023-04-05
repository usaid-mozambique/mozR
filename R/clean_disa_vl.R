#' Clean historic DISA Viral Load dataframe loaded from historic monthly data
#'
#' @param df Dataframe of historic monthly DISA VL data before cleaning and join of metadata
#'
#' @return Final cleaned historic DISA VL dataframe
#' @export
#'
#' @examples
#'
#'  df <- clean_disa_vl()}

clean_disa_vl <- function(df){

  df <- df %>%
    drop_na(datim_uid) %>%
    select(!c(snu, psnu, sitename)) %>%

    left_join(datim_orgsuids, by = c("datim_uid" = "datim_uid")) %>%
    left_join(ajuda_site_map, by = c("datim_uid" = "datim_uid")) %>%

    mutate(
      partner = replace_na(partner, "MISAU"),
      support_type = case_when(
        partner == "MISAU" ~ "Sustainability",
        TRUE ~ as.character("AJUDA")),

      agency = case_when(
        partner == "MISAU" ~ "MISAU",
        partner == "JHPIEGO-DoD" ~ "DOD",
        partner == "ECHO" ~ "USAID",
        TRUE ~ as.character("HHS/CDC")),

      across(c(snu, psnu, sitename), ~ case_when(partner == "JHPIEGO-DoD" ~ "_Military",
                                                 TRUE ~ .))) %>%

    select(period,
           sisma_uid,
           datim_uid,
           site_nid,
           starts_with("his_"),
           snu,
           psnu,
           sitename,
           support_type,
           partner,
           agency,
           age,
           group,
           sex,
           motive,
           tat_step,
           VL,
           VLS,
           TAT)

  return(df)

}
