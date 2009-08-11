\name{filterExperiments}
\Rdversion{1.1}
\alias{filterExperiments}
\alias{FUN_filterExperiments}


\title{
Functions for nonspecific filtering in the directory structure.
}
\description{
Functions for nonspecific filtering in the directory structure at the local filesystem.
}

\usage{
filterExperiments(path = getwd(), ...)

FUN_filterExperiments(esetfile, reprocess = FALSE, ...)
}

\arguments{
  \item{path}{The path variable for the data structure.}
  \item{\dots}{Further parameter to pass to the 'FUN_filterExperiments' function.}
  \item{esetfile}{The Rdata object in the data structure at the local file system with the 
  expressionSet object 'eSet'}
  \item{reprocess}{A logical, if the data should be reprocessed, even if the preprocessed 
  data object already exists in the file structure.}

}

\author{Markus Schmidberger \email{schmidb@ibe.med.uni-muenchen.de}}

\examples{
\dontrun{
	library(ArrayExpressDataManage)
	dir <- tempdir()
	data <- list(example=c("E-GEOD-8003", "E-GEOD-10097"))
	path <- createDataStruct(path = dir, data = data, name = "ArrayExpressData")
	preprocessExperiments(path = path, preprocessFUN = rma)
	filterExperiments(path = path)
}
}

\keyword{manip}
\keyword{programming}
\keyword{IO}
\keyword{file}
\keyword{utilities}
