% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rctutils.R
\name{get_mds}
\alias{get_mds}
\title{get_mds}
\usage{
get_mds(x, k, ...)
}
\arguments{
\item{x}{The object to run [limma::plotMDS()] on.}

\item{k}{The number of MDS dimensions to return. If not specified,
the maximum possible number will be returned.}

\item{...}{Additional arguments to [limma::plotMDS()].}
}
\value{
A matrix with `k` columns, and `ncol(x)` rows containing
    the MDS dimensions, with each column named "DimN", where N is
    the number of that dimension.
}
\description{
Get a table of MDS values, with proper column names.
}
\details{
This runs [limma::plotMDS()], but suppresses the generation of the
plot and instead returns the MDS dimensions in a matrix.
}
\examples{

# TODO Steal from plotMDS

}
\author{
Ryan Thompson, \email{rct@thompsonclan.org}
}
