%% -*- Coding: utf-8 -*-
\encoding{utf-8}
\name{cont.desc}
\alias{cont.desc}
\alias{print.cont.desc}
\title{Description of continuous variables}
\description{Functions to describe continuous variables }
\usage{
cont.desc(x, y = NULL, ..., labels = NULL, data = NULL)
\method{print}{cont.desc}(x, ...)
}
\arguments{
  \item{x}{ Object of type \code{formula}, \code{matrix} or
    \code{data.frame}}
  \item{y}{If x is a formula,\code{y} or \code{data} contains the data
    from x or are set to NULL if the variables are in the main
    environment}
  \item{...}{used for compatibility}
  \item{labels}{Optional vector of labels the same length as the
    dimension of x or the number of variables in formula}
  \item{data}{see y for details}
}
\value{
  An object of type "data.frame" and "cont.desc" containing
  the number of variables, the number of missing variables,
  the mean, standard deviation, min and max. 
}
\details{
  The \code{cont.desc} methods returns an object of type \code{cont.desc}
  object with a \code{print} methods associated. 
}
\author{Charles-\enc{Édouard}{Edouard} \enc{Giguère}{Giguere}}
\examples{
Ex <- data.frame(test1 = c(2, 4, NA, 3, NA),
                 test2 = c(2, 3, 5, NA, 4),
                 test3 = c(1, 4, 10, 23, NA))

### Applied on a data.frame.
cont.desc(Ex)

### Applied using a formula.
cont.desc(~ test1 + test2, Ex)

}
\keyword{description}
\keyword{continuous}
\keyword{cont.desc}
