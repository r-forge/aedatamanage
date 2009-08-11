\name{qa}
\Rdversion{1.1}
\alias{qa}
\alias{qaComplete}
\alias{qaFUNaffyPara}

\title{
Function for the quality control of all microarray data in the data structure. 
}
\description{
Function for the quality control of all microarray data in the data structure. 
}
\usage{
qaComplete(path = getwd(), qaFUN = boxplot, inputQaFun = c("affybatch", "listCELfiles"), tmpdir = tempdir(), reprocess = FALSE, file_add = "_qa.Rdata", ...)

qaFUNaffyPara(celfiles, ...)
}

\arguments{
  \item{path}{The path variable for the data structure.}
  \item{qaFUN}{The selected function for quality assessment}
  \item{inputQaFun}{The input object for the 'qaFUN' function: "affybatch" or "listCELfiles"}
  \item{tmpdir}{A path to a temporary directory, to extract the raw data.}
  \item{reprocess}{A logical, if the data should be reprocessed, even if the preprocessed 
  data object already exists in the file structure.}
  \item{file_add}{An add-on string, which will be added to flag the Rdata object in the data structure 
  at the local file system.}
  \item{\dots}{Further parameter to pass to the 'qaFun' function.}
  
  \item{celfiles}{A list of celfiles.}
}

\details{
A very useful package for the quality control is 'arrayQualityMetrics' package. This package 
can be used for the quality control of the experiments in the data structure.

A function with the selected quality assessment method(s) has to be defined and the list 
of outliers must be the return values. Then the function qaComplete() for the complete 
quality control can be used.
}

\value{
A list with a lot of information about the array quality. In the slot 'out' the 
names of the arrays with bad quality are listed.}

\author{Markus Schmidberger \email{schmidb@ibe.med.uni-muenchen.de}}

\examples{
\dontrun{
	library(ArrayExpressDataManage)
	dir <- tempdir()
	data <- list(example=c("E-GEOD-8003", "E-GEOD-10097"))
	path <- createDataStruct(path = dir, data = data, name = "ArrayExpressData")
	
	qaFunBox <- function(x)
	{
		library(arrayQualityMetrics)
		obj <- x
		prepdata<- aqm.prepdata(expressionset = obj, do.logtransform=T)
		bo<- aqm.boxplot(obj, dataprep=prepdata)
		return(bo$outliers$mean)
	}
	
	qa <- qaComplete(path = path, qaFun = qaFunBox)
}
}

\keyword{manip}
\keyword{programming}
\keyword{IO}
\keyword{file}
\keyword{utilities}
