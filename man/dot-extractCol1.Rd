% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractCol.R
\name{.extractCol1}
\alias{.extractCol1}
\title{Function  .extractCol1}
\usage{
.extractCol1(dflist, colName)
}
\arguments{
\item{dflist}{A list of data.frames which all have the same colnames and same row counts.
The dataframes in the list should have rownames (geneIDs).}

\item{colName}{The name of the data column to extract to a matrix}
}
\value{
A dataframe containing the extracted columns
}
\description{
Take a named list of dataframes where each dataframe has the same
column names (typically a list of topTable dataframes). Extract
the named column from each dataframe and return a matrix.
}
\details{
The common use case for this is to provide a list of topTable
data frames and extract one column from each file to create
a matrix of LogRatios or Pvalues.

NOTE: all dataframes must have the same rownames and roworder
}
\examples{
MyPvalues  = ExtractCol1 (TopTableList, colName="P.Value")

}
\author{
John Thompson, \email{john.thompson@bms.com}
}
\keyword{topTable}
