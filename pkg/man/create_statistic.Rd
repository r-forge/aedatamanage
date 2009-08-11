\name{create_statistic}
\Rdversion{1.1}
\alias{create_statistic}
\alias{create_overview}

\title{
Functions to create some statistic and overview table about the experiments in 
the directory structure.
}
\description{
Functions to create some statistic and overview table about the experiments in 
the directory structure.
}
\usage{
create_statistic(path = getwd(), criteria = list(ChipType = function(adf) sum(adf$Array.Design.REF == "A-AFFY-33")), verbose = getOption("verbose"))
create_overview(path=getwd(), printtype="no", printfilename=paste(basename(getwd()), printtype, sep="."  ),	chipType="A-AFFY-33", maxDesc=400,	verbose=getOption("verbose"))
}

\arguments{
  \item{path}{a character vector containing a single path name. In default the actual working directory 
  is used.}
  \item{criteria}{a list of function to evaluate some special columns of the annotated data frame.}
  \item{verbose}{logical value for some more output.}
  \item{printtype}{Type of table to produce.  Possible values for 'type' are
          '"latex"' or '"html"'. Default value is '"latex"'.}
  \item{printfilename}{Name of file where the resulting code should be saved.  If
          'file=""', output is displayed on screen.  Note that the
          function also (invisibly) returns a character vector of the
          results (which can be helpful for post-processing). Default
          value is '""'.}
  \item{chipType}{character for the chip type to use in overview table.}
  \item{maxDesc}{Length of the description filed}
}

\value{
  \item{create_statistic}{a data frame with the statistic}
  \item{reate_overview}{a data frame or a xtable object (printtype=="html" or printtype=="latex")}
}

\author{Markus Schmidberger \email{schmidb@ibe.med.uni-muenchen.de}}

\examples{
\dontrun{
	library(ArrayExpressDataManage)
	dir <- tempdir()
	data <- list(example=c("E-GEOD-8003", "E-GEOD-10097"))
	path <- createDataStruct(path = dir, data = data, name = "ArrayExpressData")
	
	stat <- create_statistic(path = path)
	info <- create_overview(path=path)
	}
}

\keyword{manip}
\keyword{programming}
\keyword{IO}
\keyword{file}
\keyword{utilities}