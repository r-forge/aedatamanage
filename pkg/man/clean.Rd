\name{clean}
\Rdversion{1.1}
\alias{clean}

\title{
Function to clean the local data structure from not original data, e.g. some calculated data.
}
\description{
After a lot of work it could be very useful to clean the data structure from temporary 
saved results:
}
\usage{
clean(path = getwd(), pattern = c(".Rdata", "sdrf.c.txt", ".pdf", ".html", ".cel", "~"))
}

\arguments{
  \item{path}{a character vector containing a single path name. In default the actual working directory 
  is used.}
  \item{pattern}{a vector of file extensions which should be removed from the file structure}
}

\author{Markus Schmidberger \email{schmidb@ibe.med.uni-muenchen.de}}


\examples{
\dontrun{
	library(ArrayExpressDataManage)
	dir <- tempdir()
	data <- list(example=c("E-GEOD-8003", "E-GEOD-10097"))
	path <- createDataStruct(path = dir, data = data, name = "ArrayExpressData")
	
	clean(path = path)
}
}

\keyword{manip}
\keyword{programming}
\keyword{IO}
\keyword{file}
\keyword{utilities}
