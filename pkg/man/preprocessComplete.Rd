\name{preprocess}
\Rdversion{1.1}
\alias{preprocess}
\alias{preprocessComplete}
\alias{preprocessGroups}
\alias{preprocessExperiments}
\alias{preprocessFUNrmaPara}

\title{
Functions to preprocess the microarray data in the data structure.
}
\description{
Functions to preprocess the microarray data in the data structure. One function 
to preprocess all data together, one function to preprocess the groups and one 
function to preprocess every experiment separate.
}

\usage{
preprocessComplete(path = getwd(), preprocessFUN = rma, 
	inputPreprocessFun = c("affybatch", "listCELfiles"), tmpdir = tempdir(), 
	reprocess = FALSE, file_add = "_eset.Rdata", ...)
preprocessGroups(path = getwd(), preprocessFUN = rma, 
	inputPreprocessFun = c("affybatch", "listCELfiles"), tmpdir = tempdir(), 
	complete = TRUE, reprocess = FALSE, ...)
preprocessExperiments(path = getwd(), preprocessFUN = rma, 
	inputPreprocessFun = c("affybatch", "listCELfiles"), tmpdir = tempdir(), 
	reprocess = FALSE, ...)

preprocessFUNrmaPara(celfiles, ...)
}


\arguments{
  \item{path}{The path variable for the data structure.}
  \item{preprocessFUN}{A function, which is used for preprocessing: e.g. rma, vsn, preprocessFUNrmaPara}
  \item{inputPreprocessFun}{The type of input object for the 'preprocessFUN' function: "affybatch" or "listCELfiles"}
  \item{tmpdir}{A path to a temporary directory, to extract the raw data.}
  \item{reprocess}{A logical, if the data should be reprocessed, even if the preprocessed 
  data object already exists in the file structure.}
  \item{file_add}{An add-on string, which will be added to flag the Rdata object in the data structure 
  at the local file system.}
  \item{complete}{A logical, if TRUE, all groups and all data together are preprocessed.}
  \item{celfiles}{A list of celfile names}
  \item{\dots}{Further parameter to pass to the 'preprocessFUN' function.}
}
\details{
The results are saved as Rdata object in the file structure.
The function 'preprocessFUNrmaPara()' is a prepared wrapper for a parallel preprocessing 
with the affyPara package.
}
\value{
A logical if everything was working fine.
}

\author{Markus Schmidberger \email{schmidb@ibe.med.uni-muenchen.de}}


\examples{
\dontrun{
	library(ArrayExpressDataManage)
	dir <- tempdir()
	data <- list(example=c("E-GEOD-8003", "E-GEOD-10097"))
	path <- createDataStruct(path = dir, data = data, name = "ArrayExpressData")
	preprocessExperiments(path = path, preprocessFUN = rma)
}
}

\keyword{manip}
\keyword{programming}
\keyword{IO}
\keyword{file}
\keyword{utilities}