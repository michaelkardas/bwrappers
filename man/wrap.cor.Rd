% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrap.cor.R
\name{wrap.cor}
\alias{wrap.cor}
\title{Correlation test}
\usage{
wrap.cor(dv1, dv2)
}
\arguments{
\item{dv1, dv2}{Column vectors containing the dependent variables}
}
\description{
Tests for a correlation between two dependent variables. The function
delegates the primary computations to \code{\link[stats]{cor.test}}.
}
\examples{
wrap.cor(dv1 = bdata$DV3_T1, dv2 = bdata$DV3_T2)

}
\seealso{
\code{\link[stats]{cor.test}}
}
