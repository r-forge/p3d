\name{Ell3d}
\Rdversion{1.1}
\alias{Ell3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw ellipsoids
%%  ~~function to do ... ~~
}
\description{
So far, Ell3d() will only draw data ellipsoids around displayed data.
}
\usage{
Ell3d()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{center}{
%%     ~~Describe \code{center} here~~
}
  \item{shape}{
%%     ~~Describe \code{shape} here~~
}
  \item{radius}{
%%     ~~Describe \code{radius} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{render}{
%%     ~~Describe \code{render} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

library(p3d)
library(car)

Init3d()
Plot3d( women ~ education + prestige | type, Prestige)
Ell3d()


## The function is currently defined as
function( center = rep(0,3), shape = diag(3), radius = 1, n = 30, render = 'wire', ...) {
      ellip <- ellipsoid( center = center, shape = shape, radius = radius, n = n)
      if ( render == 'wire' ) wire3d( ellip, ...)
      else shade3d( ellip, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
