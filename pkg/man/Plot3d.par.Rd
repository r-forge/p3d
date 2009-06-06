\name{Plot3d.par}
\Rdversion{1.1}
\alias{Plot3d.par}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Plot3d.par(..., default = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{default}{
%%     ~~Describe \code{default} here~~
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
function(..., default = FALSE ) {
    if( default ) assign(".Plot3d.par",
        list(),    # default
        pos = 1)
   # if ( exists(".tt") ) pars <- .tt
   if ( exists(".Plot3d.par") ) pars <- .Plot3d.par
    else pars <- list()
     #    disp(pars)
    if (length( a <- list(...)) == 0) return(pars)
    if (is.null(names(a))) return( pars[unlist(a)] )
    old <- pars[names(a)]
    pars[names(a)] <- a
    assign(".Plot3d.par", pars, pos = 1)
    old
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
