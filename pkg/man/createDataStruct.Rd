\name{createDataStruct}
\Rdversion{1.1}
\alias{createDataStruct}

\title{
Function to create the data structure. Using the ArrayExpress package data are downloaded 
from the ArrayExpress data base.
}
\description{
This function creates the data structure at the local file system and downloads the 
files from the AE repository.
}
\usage{
createDataStruct(path = getwd(), data = list(), name = "ArrayExpressData")
}

\arguments{
  \item{path}{a character vector containing a single path name. In default the actual working directory 
  is used.}
  \item{data}{a list object with the IDs of the AE experiments. The experiments can be grouped to 
  cohorts.}
  \item{name}{a string for the study name, this gets the name of the top directory in the file system.}
}

\value{
A character vector containing the single path name of the directory structure.
}

\author{Markus Schmidberger \email{schmidb@ibe.med.uni-muenchen.de}}

\examples{
\dontrun{
	library(ArrayExpressDataManage)
	dir <- tempdir()
	data <- list(example=c("E-GEOD-8003", "E-GEOD-10097"))
	path <- createDataStruct(path = dir, data = data, name = "ArrayExpressData")
}
}

\keyword{manip}
\keyword{programming}
\keyword{IO}
\keyword{file}
\keyword{utilities}
