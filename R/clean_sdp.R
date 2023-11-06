#' Clean SDP data from PSM
#'
#' @param SDP_data data from SPM relating to Supply Chain
#'
#' @return cleaned SDP data tibble
#' @export
#'
#' @examples
#'   \dontrun{
#'    clean_sdp(sdp_data)
#' }

clean_sdp <- function(SDP_data){
  SDP_data_df <- SDP_data %>%
    janitor::clean_names() %>%
    dplyr::select(period_lmis = lmis_date,
           orgunituid,
           product,
           Latitude = lat,
           Longitude = lon,
           soh, mos, ami, ami_tx_cons,
           stock_stat = stock_status) %>%
    dplyr::mutate(soh = as.numeric(soh),
           mos = as.numeric(mos),
           ami = as.numeric(ami),
           ami_tx_cons = as.numeric(ami_tx_cons),
           period_lmis = as.Date(as.numeric(period_lmis), origin = "1899-12-30")
    ) %>%

    return(SDP_data_df)

}

