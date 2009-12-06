\name{ell.conj}
\Rdversion{1.1}
\alias{ell.conj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find conjugate axes or tangent lines to ellipse
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ell.conj(center, shape, dir, radius = 1, len = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{center}{
%%     ~~Describe \code{center} here~~
}
  \item{shape}{
%%     ~~Describe \code{shape} here~~
}
  \item{dir}{
%%     ~~Describe \code{dir} here~~
}
  \item{radius}{
%%     ~~Describe \code{radius} here~~
}
  \item{len}{
%%     ~~Describe \code{len} here~~
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
function( center, shape, dir, radius = 1, len = 1) {
            # returns conjugate axes or tangent lines to ellipse
            vecs <- uv( shape, dir, radius)
            list( u = list( center-len*vecs$u, center+len*vecs$u),
                  v = list( center-len*vecs$v, center+len*vecs$v),
                  tan1 = list( center + vecs$u - len*vecs$v,center + vecs$u+ len*vecs$v),
                  tan2 = list( center - vecs$u - len*vecs$v,center - vecs$u+ len*vecs$v),
                  tan3 = list( center + vecs$v - len*vecs$u,center + vecs$v+ len*vecs$u),
                  tan4 = list( center - vecs$v - len*vecs$u,center - vecs$v+ len*vecs$u),
                  center = center)
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
