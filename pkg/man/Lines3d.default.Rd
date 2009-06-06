\name{Lines3d.default}
\Rdversion{1.1}
\alias{Lines3d.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Lines3d.default(..., col = "blue")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
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
function( ..., col = 'blue' )  {
    a <- args3d(..., col=col)
    xyz <- a$x
    if (nrow(xyz) == 0) return( invisible(0))
    nas <- apply(xyz, 1, function(x) any(is.na(x)))
    inds <- 1:nrow(xyz)
    inds[nas] <- NA
    inds <-  cbind(inds[-length(inds)],inds[-1])
    inds <- na.omit(inds)
    inds <- c(t(inds))
    xyz <- xyz[inds,]
    a$x <- xyz
    names(a) <- sub('^lwd$','size', names(a))
    # do.call("rgl.lines", a)
    do.call("segments3d", a)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
