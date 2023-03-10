% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reshape_em_imer.R
\name{reshape_em_imer}
\alias{reshape_em_imer}
\title{Process monthly enhanced monitoring IMER submission from PEPFAR Mozambique Clinical Partners}
\usage{
reshape_em_imer(filename, ip)
}
\arguments{
\item{filename}{Local path to the monthly IP submission}

\item{ip}{IP whose submission the file pertains to}
}
\value{
A tidy dataframe with monthly enhanced monitoring IMER results
}
\description{
Process monthly enhanced monitoring IMER submission from PEPFAR Mozambique Clinical Partners
}
\examples{
\dontrun{

df <- reshape_em_imer()}
}
