% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrap.split.R
\name{wrap.split}
\alias{wrap.split}
\title{Split a data frame}
\usage{
wrap.split(df, iv1, iv2 = NULL, iv3 = NULL)
}
\arguments{
\item{df}{The data frame that you intend to split}

\item{iv1, iv2, iv3}{Names of the columns containing the independent variables}
}
\description{
Splits a data frame across all combinations of 1-3 independent
variables and then assigns the new data frames directly to the Global Environment.
The function delegates the primary computations to \code{\link[base]{split}}.
}
\examples{
## Splitting a data frame by 1 independent variable
wrap.split(df = bdata, iv1 = IV1)

## Splitting a data frame by 2 independent variables
wrap.split(df = bdata, iv1 = IV1, iv2 = IV2)
}
\seealso{
\code{\link[base]{split}}
}
