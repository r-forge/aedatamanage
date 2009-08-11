\name{correctBatch}
\Rdversion{1.1}
\alias{correctBatch}
\alias{FUN_correctBatch}

\title{
Function to correct Batch Effect in ExpressionSet object in data structure.
}
\description{
Combining experiments from different labs in most cases involves a so called batch effect, 
which have to be removed. This function uses the ComBat method to remove the batch effect.
}
\usage{
correctBatch(path = getwd(), ...)

FUN_correctBatch(file, reprocess = FALSE, saveold = FALSE)

}

\arguments{
  \item{path}{The path variable for the data structure.}
  \item{\dots}{Further parameter to pass to the 'ComBat' function.}
  \item{file}{Name of the Rdata object at the data structure at the local file system.}
  \item{reprocess}{A logical, if the data should be reprocessed, even if the preprocessed 
  data object already exists in the file structure.}
  \item{saveold}{A logical, if old results (not batch corrected) should be saved}
}

\value{
A logical, if everything is working correct.
}

\author{Markus Schmidberger \email{schmidb@ibe.med.uni-muenchen.de}}

\examples{
\dontrun{
	library(ArrayExpressDataManage)
	dir <- tempdir()
	data <- list(example=c("E-GEOD-8003", "E-GEOD-10097"))
	path <- createDataStruct(path = dir, data = data, name = "ArrayExpressData")
	preprocessExperiments(path = path, preprocessFUN = rma)
	correctBatch(path=path)
}
}

\keyword{manip}
\keyword{programming}
\keyword{IO}
\keyword{file}
\keyword{utilities}
