#' Process monthly enhanced monitoring submission datasets from PEPFAR Mozambique Clinical Partners
#' @param df Processed monthly enhanced monitoring dataframe
#' @param type Submission tab
#' @return A tidy dataframe with monthly enhanced monitoring results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- clean_em()}

clean_em <- function(df, type){

  if(type == "TPT"){
    df <- clean_em_tpt(df)
  } else if (type == "TXTB"){
    df <- clean_em_txtb(df)
  } else if (type == "MI"){
    df <- clean_em_mi(df)
  } else if (type == "DSD"){
    df <- clean_em_dsd(df)
  } else if (type == "IMER"){
    df <- clean_em_imer(df)
  } else if (type == "PrEP"){
    df <- clean_em_prep(df)
  }

  return(df)

}
