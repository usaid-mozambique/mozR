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

clean_em <- function(type, filename, ip){

  if(type == "TPT"){
    df <- clean_em(filename, ip)
  } else if (type == "TXTB"){
    df <- clean_em(filename, ip)
  } else if (type == "MI"){
    df <- clean_em(filename, ip)
  } else if (type == "DSD"){
    df <- clean_em(filename, ip)
  } else if (type == "IMER"){
    df <- clean_em(filename, ip)
  } else if (type == "PrEP"){
    df <- clean_em(filename, ip)
  }

  return(df)

}
