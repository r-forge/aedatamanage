\name{dapply}
\Rdversion{1.1}
\alias{dapply}

\title{
Apply Function Over Files in a Path
}

\description{
Returns a list of values obtained by applying a function to files in a path. The files can be 
selected with regular expression matching. This is a simple wrapper around the lapply() function.
}

\usage{
dapply(FUN, path = getwd(), pattern = NULL, recursive = TRUE, ...)
}


\arguments{
  \item{FUN}{the function to be applied. First of all the function should read the file.}
  \item{path}{a character vector of full path names; the default corresponds to the working directory 'getwd()'. Missing values will be ignored.}
  \item{pattern}{an optional regular expression. Only file names which match the regular expression will be applied to the function.}
  \item{recursive}{logical. Should the listing recurse into directories?}
  \item{\dots}{optional arguments to lapply() and FUN.}
}

\value{
A list.
}
\author{Markus Schmidberger \email{schmidb@ibe.med.uni-muenchen.de}}

\examples{
library(ArrayExpressDataManage)
path <- system.file("doc", package="ArrayExpressDataManage")
dapply(print, path=path)
}

\keyword{manip}
\keyword{programming}
\keyword{IO}
\keyword{file}
\keyword{utilities}