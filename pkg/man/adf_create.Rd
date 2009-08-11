\name{adf_create}
\Rdversion{1.1}
\alias{adf_create}
\alias{read.AnnotatedDataFrame.AE}

\title{
Function to create adf object from sdrf files in directory structure and 
to read the sdrf files
}

\description{
Function to create adf (annotated data frame) object from sdrf files in 
directory structure and to read the sdrf (Sample and Data Relationship Format) files
}

\usage{
adf_create(sdrffiles, verbose = getOption("verbose"))

read.AnnotatedDataFrame.AE(filename, path, row.names = NULL, 
	blank.lines.skip = TRUE, fill = TRUE, varMetadata.char = "$", ...)
}

\arguments{
  \item{sdrffiles}{A list of sdrf files.}
  \item{verbose}{A logical parameter to get some output.}
  \item{filename}{A sdrf file name from which to read.}
  \item{path}{(optional) directory in which to find 'filename'.}
  \item{row.names}{this argument gets passed on to 'read.table' and will be
          used for the row names of the phenoData slot.}
  \item{blank.lines.skip}{logical: if 'TRUE' blank lines in the input are
          ignored.}
  \item{fill}{logical. If 'TRUE' then in case the rows have unequal length,
          blank fields are implicitly added.}
  \item{varMetadata.char}{lines beginning with this character are used for the
          'varMetadata' slot. See examples.}
  \item{\dots}{Further parameters to pass to 'read.table'}
}

\value{
An AnnotatedDataFrame object.
}

\author{Markus Schmidberger \email{schmidb@ibe.med.uni-muenchen.de}}

\examples{
\dontrun{
	library(ArrayExpressDataManage)
	dir <- tempdir()
	data <- list(example=c("E-GEOD-8003", "E-GEOD-10097"))
	path <- createDataStruct(path = dir, data = data, name = "ArrayExpressData")
	sdrffiles <- list.files(path, pattern="sdrf", recursive=TRUE)
	adf <- adf_create(sdrffiles)
}
}

\keyword{manip}
\keyword{programming}
\keyword{IO}
\keyword{file}
\keyword{utilities}
