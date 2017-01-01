% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runVoom.R
\name{runVoom}
\alias{runVoom}
\title{Function  runVoom (voom then lmFit)}
\usage{
runVoom(dgeObj, designMatrix, formula, dupcorBlock, qualityWeights = TRUE,
  var.design, mvPlot = TRUE, runEBayes = FALSE, proportion = 0.01)
}
\arguments{
\item{dgeObj}{A DGEobj containing a DGEList (e.g. from runEdgeRNorm) (required)}

\item{designMatrix}{A design matrix created by model.matrix (required)}

\item{formula}{A text representation of a formula to fit (required)}

\item{dupcorBlock}{Supply a block argument to trigger duplicateCorrelation (optional). 
Should be a vector the same length as ncol with values to indicate common
group membership for duplicateCorrelation.}

\item{qualityWeights}{Runs VoomWithQualityWeights if set to TRUE (default=TRUE).
This should normally be set to TRUE.}

\item{var.design}{Provide a design matrix (from model.matrix) to identify 
replicate groups (e.g. "~ ReplicateGroup") for quality weight determination. 
Causes quality weights to be determined on a group basis.  If omitted 
VoomWithQualityWeights treats each sample individually.}

\item{mvPlot}{Enables the voom mean-variance plot (Default = TRUE)}

\item{runEBayes}{Runs eBayes after lmfit; default = FALSE}

\item{proportion}{Proportion of genes expected to be differentially expressed
(used by eBayes) (Default = 0.01) Modify the prior accordingly if your 1st pass analysis shows 
significantly higher or lower proportion of genes regulated than the default.
a significantly higher or lower proportion of genes regulated than the default.}
}
\value{
A DGEobj now containing designMatrix, Elist and fit object
}
\description{
In the recommended workflow this runs voomWithQualityWeights followed by
lmfit and optionally eBayes.  You should enable eBayes if the contrasts of
interest are already represented in the model. If you intend to use
contrasts.fit, you should run eBayes after that step instead.
}
\details{
Input is minimally a DGEobj containing a DGEList (typically TMM-normalized), 
a design matrix (from model.matrix) and a formula (text
representation).  Other arguments can invoke duplicateCorrelation and modify
use of quality weights.

Returns a DGEobj class object containing the designMatrix, VoomElist (voom
output) and Fit object (lmfit output). Appends data items to the input
DGEobj.

Quality weights should be left enabled unless you have a good reason to turn it 
off.  If all samples are equal quality, the weights will all approach 1.0 with no 
consequence on the results.  More typically,  some samples are better than others
and using quality weights improves the overall result.

Use var.design when you notice that quality weights are correlated with some
factor in the experiment.  This will cause the quality weights to be 
calculated as a group instead of individually.

Use duplicate correlattion when you have subjects that have been sampled more
than once (e.g. before and after some treatment).  This calculates a within 
subject correlation and includes this in the model.
}
\examples{

}
\author{
John Thompson, \email{john.thompson@bms.com}
}
\keyword{Entrez,}
\keyword{GeneID}
\keyword{gene}
\keyword{symbol,}
