\name{Plot3d}
\Rdversion{1.1}
\alias{Plot3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Plot3d(x, y, z, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{z}{
%%     ~~Describe \code{z} here~~
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
function( x, y, z, ...) {
'
    Help summary:
    Plot3d ( y ~ x + z [ |g ], data, groups = id,
        sphere.size = 1,
        xlim,   # across screen
        ylim,   # vertical
        zlim,   # into screen
        xlab,
        ylab,
        zlab,
        xtend = .05, # axes extend beyond range of data
        debug = T,
        revolutions = 0,
        bg.col = c("white", "black"),
        axis.col = if (bg.col == "white") "black" else "white",
        surface.col = c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"),
        neg.res.col = "red",
        pos.res.col = "green",
        point.col = "yellow",
        text.col = axis.col,
        grid.col = if (bg.col == "white") "black" else "gray",
        fogtype = c("none","exp2", "linear", "exp"),
        residuals = (length(fit) == 1),
        surface=FALSE,
        fill=TRUE,
        grid=TRUE,
        grid.lines=26,
        df.smooth=NULL,
        df.additive=NULL,
        threshold=0.01,
        speed=1,
        fov=30,
        fit="linear",
        groups=NULL,
        parallel=TRUE,
        ellipsoid=FALSE,  # data ellipsoid within
        level=0.5,
        model.summary=FALSE,
        verbose = 0,
        clear = T,
        pow = -1,
        origin = F,
        axes = T,
        lines.col = "gray",
        lines.lwd= 1)
'
    UseMethod("Plot3d")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
