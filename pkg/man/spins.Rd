\name{spins}
\Rdversion{1.1}
\alias{spins}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spins(inc.theta = 1/4, inc.phi = 0, theta = NULL, phi = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inc.theta}{
%%     ~~Describe \code{inc.theta} here~~
}
  \item{inc.phi}{
%%     ~~Describe \code{inc.phi} here~~
}
  \item{theta}{
%%     ~~Describe \code{theta} here~~
}
  \item{phi}{
%%     ~~Describe \code{phi} here~~
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
