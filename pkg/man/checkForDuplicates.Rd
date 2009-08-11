\name{checkForDuplicates}
\Rdversion{1.1}
\alias{checkForDuplicates}

\title{
Function to check for duplicate arrays in all experiment data.
}
\description{
Using a lot of experiments from the AE database, there can be duplicate arrays in different 
experiments. These arrays have to be removed from the analysis.
}
\usage{
checkForDuplicates(path, tmpdir = tempdir(), verbose = getOption("verbose"))
}

\arguments{
  \item{path}{a character vector containing a single path name. In default the actual working directory 
  is used.}
  \item{tmpdir}{a character vector containing a single path name, where packed raw data should be 
  extracted. In default a temporary directory is used.}
  \item{verbose}{logical value for some more output.}
}

\value{
a list of the duplicate array names
}

\author{Markus Schmidberger \email{schmidb@ibe.med.uni-muenchen.de}}

\examples{
\dontrun{
	library(ArrayExpressDataManage)
	dir <- tempdir()
	data <- list(example=c("E-GEOD-8003", "E-GEOD-10097"))
	path <- createDataStruct(path = dir, data = data, name = "ArrayExpressData")
	
	duplicates <- checkForDuplicates(path = path)
	}
}

\keyword{manip}
\keyword{programming}
\keyword{IO}
\keyword{file}
\keyword{utilities}
