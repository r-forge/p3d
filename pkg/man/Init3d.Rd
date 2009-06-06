\name{Init3d}
\Rdversion{1.1}
\alias{Init3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Init3d(par3d = list(), family = c("sans", "serif", "mono", "symbol"), cex = 0.8, font = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par3d}{
%%     ~~Describe \code{par3d} here~~
}
  \item{family}{
%%     ~~Describe \code{family} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{font}{
%%     ~~Describe \code{font} here~~
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
function( par3d = list(),
      family = c('sans','serif', "mono", "symbol"),
      cex = .8,
      font = 1,
      ...) {
    family = match.arg(family)
    mod = list(family=family, cex = cex, font = font)
    par3d [ names(mod)] <- mod
    require(rgl)
    require(mgcv)
    rgl.open()
    do.call('par3d', par3d)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
