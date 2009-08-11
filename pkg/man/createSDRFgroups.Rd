\name{createSDRFgroups}
\Rdversion{1.1}
\alias{createSDRFgroups}
\alias{FUN_write_AnnotatedDataFrame}
\alias{write.AnnotatedDataFrame}

\title{
Functions to create and write sdrf files for groups in directory structure
}
\description{
Functions to create and write sdrf (Sample and Data Relationship Format) 
files for groups in the directory structure at the local file system.
}
\usage{
createSDRFgroups(path = getwd(), ...)

FUN_write_AnnotatedDataFrame(file, reprocess = FALSE, remove = c("Comment..Derived.ArrayExpress.FTP.file.", "Derived.Array.Data.Matrix.File", "Comment..Derived.ArrayExpress.Data.Retrieval.URI.", "Comment..ArrayExpress.FTP.file.", "Comment..ArrayExpress.Data.Retrieval.URI.", "Comment..Array.Design.URI.", "Sample.Name", "Scan.Name", "Protocol.REF", "Extract.Name", "Material.Type", "Term.Source.REF", "LabeledExtract.Name", "Label"))
write.AnnotatedDataFrame(adf, file = "", sep = "\t", row.names = FALSE, quote = FALSE, col.names = TRUE)
}

\arguments{
  \item{path}{The path variable for the data structure.}
  \item{\dots}{Further parameter to pass to the 'FUN_write_AnnotatedDataFrame' function.}
  
  \item{file}{Name of the Rdata object at the data structure at the local file system.}
  \item{reprocess}{A logical, if the data should be reprocessed, even if the preprocessed 
  data object already exists in the file structure.}
  \item{remove}{A vector of comlumns, which should be removed from the adf}
  
  \item{adf}{The annotated data frame object}
  \item{sep}{the field separator string.  Values within each row of 'adf' are separated by this string.}
  \item{row.names}{either a logical value indicating whether the row names of 
  'adf' are to be written along with 'adf', or a character vector of row names to be written.}
  \item{quote}{a logical value ('TRUE' or 'FALSE') or a numeric vector.  If 
  'TRUE', any character or factor columns will be surrounded by double quotes. If a 
  numeric vector, its elements are taken as the indices of columns to quote. In both 
  cases, row and column names are quoted if they are written.  If 'FALSE', nothing is quoted.}
  \item{col.names}{either a logical value indicating whether the column names of 'adf' are 
  to be written along with 'adf', or a character vector of column names to be written. 
  See the section on 'CSV files' for the meaning of 'col.names = NA'.}
}

\author{Markus Schmidberger \email{schmidb@ibe.med.uni-muenchen.de}}

\examples{
\dontrun{
	library(ArrayExpressDataManage)
	dir <- tempdir()
	data <- list(example=c("E-GEOD-8003", "E-GEOD-10097"))
	path <- createDataStruct(path = dir, data = data, name = "ArrayExpressData")
	createSDRFgroups(path = path)
}
}

\keyword{manip}
\keyword{programming}
\keyword{IO}
\keyword{file}
\keyword{utilities}