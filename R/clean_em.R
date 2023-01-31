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

  switch(type,
         "TPT" = clean_em_tpt(df),
         "TXTB" = clean_em_txtb(df),
         "MI" = clean_em_mi(df),
         "DSD" = clean_em_dsd(df),
         "IMER" = clean_em_imer(df),
         "PrEP" = clean_em_prep(df)
  )

  return(df)

}
