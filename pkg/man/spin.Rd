\name{spin}
\Rdversion{1.1}
\alias{spin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Spin or rotate a 3D plot
}
\description{
Like \code{rgl.viewpoint} but starts with defaults from \code{par3d},  except
for \code{theta} (rotation around vertical axis)
and \code{phi} (angular height above the horizontal plane) 

}
\usage{
spin(theta = 0, phi = 15, fov = par3d("FOV"), zoom = par3d("zoom"), scale = par3d("scale"), stay = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
rotation around vertical axis
}
  \item{phi}{
angular height above the horizontal plane
}
  \item{fov}{
%%     ~~Describe \code{fov} here~~
}
  \item{zoom}{
Zoom factor, passed to \code{link[rgl]{rgl.viewpoint}}
}
  \item{scale}{
Scale factor, passed to \code{link[rgl]{rgl.viewpoint}}
}
  \item{stay}{
Not used in this implementation. 
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
None.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
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
\code{link[rgl]{rgl.viewpoint}}
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
