\name{Axes3d}
\Rdversion{1.1}
\alias{Axes3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Axes3d(n = 5, len = 0.05, col = "black", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{len}{
%%     ~~Describe \code{len} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
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

## The function is currently defined as
function(n=5, len= .05,col = 'black', ...) {
    #
    # Draws axes
    #
    gen.axes <- function( pvals, orig, ext , len=.05)  {
        ticks <- rbind( orig, orig*(1-len) + len* (orig -( ext - orig)))
        # disp(ticks)
        ps <- c(apply(cbind( pvals, pvals, NA),1,c))
        # disp(ps)
        inds <- rep( c(1,2,NA), length(pvals))
        # disp(inds)
        ret <- cbind( ps, ticks[inds,])
        ret 
    }
    Pretty <- function(x,n=5,...) {
        ret <- pretty(x,n=n,...)
        drop <- ret < min(x) | ret > max(x)
        ret [!drop]
    }
    abox <- Plot3d.par()$abox
    
    mat <- gen.axes( labs <- Pretty( abox[1:2],n=n), abox[c(3,5)], abox[c(4,6)],len = len)
    Lines3d( xyz = mat,col=col,lwd=1,...)
    Text3d( xyz = mat[seq(2, nrow(mat), 3),], text = as.character(labs),col = col,...)
    
    mat <- gen.axes( labs <- Pretty( abox[3:4],n=n), abox[c(1,5)], abox[c(2,6)],len = len)
    Lines3d( yxz = mat , col = col,lwd=1,...)                         
    Text3d( yxz = mat[seq(2, nrow(mat), 3),], text = as.character(labs),col = col,...)
   
    mat <- gen.axes( labs <- Pretty( abox[5:6],n=n), abox[c(1,3)], abox[c(2,4)],len = len)
    Lines3d( zxy = mat , col = col,lwd=1,...)
    Text3d( zxy = mat[seq(2, nrow(mat), 3),], text = as.character(labs),col = col,...)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
