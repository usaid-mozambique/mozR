#' Parse monthly sisma csv export
#' @param data Data input to be parsed
#' @param type Data type to be parsed
#' @return A tidy dataframe with monthly sisma results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_csv()}

parse_sisma_csv <- function(data, type){

  switch(type,
         "CPN" = parse_sisma_smi_cpn(df),
         "ATS Result" = parse_sisma_ats_results(df)
  )

  return(df)

}
