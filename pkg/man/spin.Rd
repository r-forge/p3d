\name{spin}
\Rdversion{1.1}
\alias{spin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Spin a 3D plot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spin(theta = 0, phi = 15, fov = par3d("FOV"), zoom = par3d("zoom"), scale = par3d("scale"), stay = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
%%     ~~Describe \code{theta} here~~
}
  \item{phi}{
%%     ~~Describe \code{phi} here~~
}
  \item{fov}{
%%     ~~Describe \code{fov} here~~
}
  \item{zoom}{
%%     ~~Describe \code{zoom} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
}
  \item{stay}{
%%     ~~Describe \code{stay} here~~
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
Georges Monette
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
function( theta=0, phi=15,
          fov = par3d("FOV"), zoom = par3d("zoom"), 
          scale = par3d("scale"), stay = FALSE) {
          
          # Like rgl viewpoint but starts with defaults except
          # for theta (rotation around vertical axis)
          # and phi (angular height above the horisontal plane) 
          # PLAN:
          # without arguments it should just start spinning
          # USAGE: see rgl.viewpoint and rgl.snapshot
          # rgl.bringtotop(stay = FALSE)
          rgl.viewpoint( theta=theta, phi = phi,
                fov = fov, zoom = zoom, scale = scale)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
