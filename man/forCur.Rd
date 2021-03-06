% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numeric_format.R
\name{forCur}
\alias{forCur}
\alias{forCurGBP}
\alias{forCurArea}
\alias{forCurAreaGBP}
\title{Formats a numeric vector to the desired units and concommitant styling}
\usage{
forCur(x)

forCurGBP(x)

forCurArea(x)

forCurAreaGBP(x)
}
\arguments{
\item{x}{A numeric of any length}
}
\value{
A formated character string
}
\description{
Formats a numeric vector to the desired units and concommitant styling
}
\section{Functions}{
\itemize{
\item \code{forCurGBP}: Add Sterling sign

\item \code{forCurArea}: Add meter sq. signs for areas

\item \code{forCurAreaGBP}: Add Sterling and per meter sq. signs for cost per area
}}

\examples{
forCur(405556)
forCurGBP(346236)
forCurArea(452)
forCurAreaGBP(236)
}
