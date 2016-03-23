\encoding{latin1}
\name{cross}
\alias{cross}
\alias{print.cross}
\title{Crosstabs}
\description{Functions to display (2 x 2) contingency table}
\usage{
xtab(formula, data, useNA = FALSE, exclude = c(NA,NaN), miss.char = "-",
     na.action = na.exclude, subset = NULL, sparse = FALSE,
     drop.unused.levels = FALSE)
Total(x)
}
\arguments{
  \item{formula}{ Object of class \code{cross} to be printed}
  \item{data}{data frame to use with formula}
  \item{useNA}{logical values to add NA to the levels in the table}
  \item{miss.char}{Character to replace NA}
  \item{...}{For other arguments check \code{help(xtabs)}}
  \item{x}{Numerical vector to sum}
}
\value{
  \code{xtab} returns an object of type \code{table} (see
  details). Total returns a sum with \code{na.rm=TRUE} by
  default and replaces NA with 0. 
}
\details{
  The \code{xtab} functions corrects the inability to deal with missing
  values in the original \code{xtabs} that comes with R base. Total is
  a utility function to use in conjunction with addmargins instead of sum.
}
\author{ \enc{Charles-Édouard}{Charles-Edouard} \enc{Giguère}{Giguere} }
\examples{
### example of crosstabs
xtab( ~ N + P, npk)
}
\keyword{table}
\keyword{cross}
