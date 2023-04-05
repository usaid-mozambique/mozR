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
         "CPN" = parse_sisma_smi_cpn(data),
         "ATS Result" = parse_sisma_ats_results(data),
         "ATS History" = parse_sisma_ats_history(data),
         "ATS CI" = parse_sisma_ats_index(data),
         "ATS SAAJ" = parse_sisma_ats_saaj_cm(data),
         "ATS CCSD" = parse_sisma_ats_ccsd(data),
         "ATS SMI" = parse_sisma_ats_smi(data),
         "ATS Auto" = parse_sisma_ats_auto(data),
         "HIV TARV" = parse_sisma_hiv_tarv(data)
  )

}
