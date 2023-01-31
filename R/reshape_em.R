#' Process monthly enhanced monitoring submission datasets from PEPFAR Mozambique Clinical Partners
#' @param type Submission tab
#' @param filename Local path to the monthly IP submission
#' @param ip IP whose submission the file pertains to
#' @return A tidy dataframe with monthly enhanced monitoring results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- reshape_em()}


reshape_em <- function(type, filename, ip){

  if(type == "TPT"){
    df <- reshape_em_tpt(filename, ip)
  } else if (type == "TXTB"){
    df <- reshape_em_txtb(filename, ip)
  } else if (type == "MI"){
    df <- reshape_em_mi(filename, ip)
  } else if (type == "DSD"){
    df <- reshape_em_dsd(filename, ip)
  } else if (type == "IMER"){
    df <- reshape_em_imer(filename, ip)
  } else if (type == "PrEP"){
    df <- reshape_em_prep(filename, ip)
  }

  return(df)

}
