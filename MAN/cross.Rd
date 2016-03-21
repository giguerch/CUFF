\encoding{latin1}
\name{cross}
\alias{cross}
\alias{print.cross}
\title{Crosstabs}
\description{Functions to display (2 x 2) contingency table}
\usage{
cross(t, ...)
\method{print}{cross}(x, ..., test = "chisq.test", export = NULL)
}
\arguments{
  \item{t}{ Object of type \code{table} or \code{formula},
    \code{vector} to tabulate}
  \item{x}{ Object of class \code{cross} to be printed}
  \item{...}{Arguments passed to \code{table} of \code{xtabs}}
  \item{test}{ list of statistical tests (as character vector) passed to
    the 2x2 table. By default, \code{test} is set to "chisq.test" which
    performs a khi-square test with Yates continuity correction.}
  \item{export}{Either "pdf" or "xlsx" or NUll. Crosstab is
    flushed into either a pdf using latex or an Excel spreadsheet using
    package openxlsx}
}
\value{
  The \code{cross} methods returns an object of type \code{cross} with the
  original table and the marginal percentages by row and by column. A
  print methods is associated with a \code{cross} object.
}
\author{ \enc{Charles-Édouard}{Charles-Edouard} \enc{Giguère}{Giguere} }
\examples{
### example of crosstabs
cr1 <- cross( ~ N + P, npk)
print(cr1, test = c("chisq.test", "fisher.test"))
}
\keyword{table}
\keyword{cross}
