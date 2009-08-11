\name{ComBat}
\Rdversion{1.1}
\alias{ComBat}

\title{
ComBat function to remove the Batch Effect.
}
\description{
Combining experiments from different labs in most cases involves a so called batch effect. 
This package provides a parametric and non-parametric empirical Bayes framework called 
'Combatting batch effects when combining batches of gene expression microarray data' (ComBat, 
http://statistics.byu.edu/johnson/ComBat). 
}
\usage{
ComBat(expression, sample_info, type = "txt", write = F, covariates = "all", par.prior = T, filter = F, skip = 0, prior.plots = T)
}

\arguments{
  \item{expression}{A expression intensity matrix}
  \item{sample_info}{A sample information matrix.  The first column must contain the names of the arrays (column names) as in your expression 
 index file. If your expression index file contains 'presence call' you should not include 
 the 'call' column names in the sample information file. The columns after Array and Sample 
 names include batch and covariate (treatment) information. The batch column is required 
 and must be named 'Batch'. Note that currently ComBat only deals with categorical covariates. 
 Numerical covariates have not been implemented at this time. See http://statistics.byu.edu/johnson/ComBat/Usage.html}
  \item{type}{File type for the expression values.}
  \item{write}{A logical, if adjusted files are written to a file or not.}
  \item{covariates}{The covariates}
  \item{par.prior}{A logical, if parametric adjustment is used}
  \item{filter}{A logical for filter for presence/absence all in the data}
  \item{skip}{The number of columns that contain probe names and gene information.}
  \item{prior.plots}{A logical, if prior plots are plotted}
}
\details{
This function uses the code from the website http://statistics.byu.edu/johnson/ComBat/Abstract.html
}
\value{
A batch corrected expression Set object
}
\references{
Johnson, WE, Rabinovic, A, and Li, C (2007). Adjusting batch effects in microarray expression data using Empirical Bayes methods. Biostatistics 8(1):118-127.
}

\author{Markus Schmidberger \email{schmidb@ibe.med.uni-muenchen.de} 
and W. Evan Johnson \email{evan@stat.byu.edu}}

\examples{
\dontrun{
	library(ArrayExpressDataManage)
	
	library(affydata)
	data(Dilution)
	eSet <- rma(Dilution)
	sample_info <- cbind(rownames(pData(e)), rownames(pData(e)), c(1,0,1,0))
	
	sample_info <- pData(eSet)
	colnames(sample_info) <- c("array", "sample", "Batch")
		
	exprs(eSet) <- ComBat( exprs(eSet), sample_info )	
}
}

\keyword{manip}
\keyword{programming}
\keyword{IO}
\keyword{file}
\keyword{utilities}
