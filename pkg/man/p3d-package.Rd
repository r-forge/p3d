\name{p3d-package}
\Rdversion{1.1}
\alias{p3d-package}
\alias{p3d}
\docType{package}
\title{
Visualization of models in 3D
}
\description{
    \code{p3d} is a very rough collection of functions to help visualize models
    that are expressed as functions of two independent variables with the
    addition of a possible categorical variable.
}
\details{
\tabular{ll}{
Package: \tab p3d\cr
Type: \tab Package\cr
Version: \tab 0.02\cr
Date: \tab 2009-06-14\cr
License: \tab GPL version 2 or newer\cr
LazyLoad: \tab yes\cr
}
A simple example illustrated the major features of \code{p3d}:

  library(p3d)
  library(car)

  data(Prestige)

  fit <- lm( women ~ education * prestige * type, Prestige)
  
  Init3d()
  Plot3d( women ~ education + prestige | type, Prestige)
  Fit3d( fit )
  Ell3d()

  snap()

}
\author{
Georges Monette

Maintainer: Georges Monette <georges@yorku.ca>
}
%\references{
%~~ Literature or other references for background information ~~
%}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\keyword{ multivariate }
\seealso{
%~~ Optional links to other man pages, e.g. ~~
\code{\link[rgl:rgl-package]{rgl}} ~~
}
\examples{
~~ simple examples of the most important functions ~~
}
\section{Changes}{
\item{Lines3d.default}{2009-05-09: disabled line changing 'lwd' to 'size' since 'segments3d' uses 'lwd'.}
}

