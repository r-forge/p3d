\name{spins}
\Rdversion{1.1}
\alias{spins}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Spin the current rgl window continuously
}
\description{
Spins the current rgl window with constant increment(s) in theta and/or phi.
Use \code{<ESC>} in the R window to stop.
}
\usage{
spins(inc.theta = 1/4, inc.phi = 0, theta = NULL, phi = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inc.theta}{
Increment for rotation about the vertical, \code{y}, axis.
}
  \item{inc.phi}{
Increment for rotation around the \code{x} axis.
}
  \item{theta}{
Initial value for \code{theta}. By default, the current orientation is used.
}
  \item{phi}{
Initial value for \code{phi}. By default, the current orientation is used.
}
}
\details{
TBW
}
\value{None}

%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Georges Monette
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{spin}},
\code{\link[rgl]{spin3d}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( inc.theta = 1/4, inc.phi = 0, theta = NULL, phi = NULL) {
 help = "
    Spins the current rgl window with a constant increment in theta and phi.
    Use ESC in R window to stop.
    BUG: - Respects some mouse motions but behaves unpredictably.
         - Should have a better way to stop
 "
      um <- par3d('userMatrix')
      Acos = function(x) 360*acos(x)/(2*pi)
      Asin = function(x) 360*asin(x)/(2*pi)
      Atan2 = function( s, c) atan2( s, c)*(180/pi)
      theta.phi = function( ) {

          par3d()
          um = par3d('userMatrix')
          list(theta = Atan2(-um[1,3], um[1,1]), phi = Atan2 (um[3,2],um[2,2]))
      }

      tp = theta.phi()
      if(is.null(theta)) theta = tp$theta
      if(is.null(phi)) phi = tp$phi
      while(TRUE) {
           theta = (theta + inc.theta) \%\% 360
           phi = phi + inc.phi
           spin( theta = theta , phi = phi)
           tp = theta.phi()
           # print( tp )
           theta = tp$theta
           phi = tp$phi
      }
      invisible(NULL)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ iplot }
\keyword{ dynamic }% __ONLY ONE__ keyword per line
