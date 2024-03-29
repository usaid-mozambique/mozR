% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_disa_vl.R
\name{process_disa_vl}
\alias{process_disa_vl}
\title{Process monthly DISA results for Viral Load}
\usage{
process_disa_vl(filename, month)
}
\arguments{
\item{filename}{Local path to the monthly DISA dataset}

\item{month}{Month of DISA results}
}
\value{
A tidy dataframe with monthly DISA results
}
\description{
Process monthly DISA results for Viral Load
}
\examples{
\dontrun{

df <- process_disa_vl()}
}
