\name{Plot3d.default}
\Rdversion{1.1}
\alias{Plot3d.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Plot3d.default(x, y, z, xlab, ylab, zlab, groups = NULL, ...)
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
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{zlab}{
%%     ~~Describe \code{zlab} here~~
}
  \item{groups}{
%%     ~~Describe \code{groups} here~~
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
function(x, y, z, xlab, ylab, zlab, groups = NULL, ...) {
   help = "
   Plot3d()   by GM Nov 9, 2005
     Modified Feb 7, 2007
   A wrapper for scat3d modified from scatter3d by John Fox
   "
   ####  NOTE: This function needs considerable work
   if ( ! is.null(groups)) stop("groups implemented only in Plot3d.formula")
   if ( is.matrix(x) || is.data.frame(x)) {
      if ( is.null( colnames( x ))) {
          colnames(x) <- paste( deparse(substitute(x)), 1:ncol(x))
      }
      if(missing(xlab)) xlab = colnames(x)[1]
      if(missing(ylab)) ylab = colnames(x)[2]
      if(missing(zlab)) zlab = colnames(x)[3]
      dd <- x[,1:3]
      names(dd) <- c(xlab,ylab,zlab)
      data <- data.frame( x[,1], x[,2], x[,3])
      names( data ) <- c(xlab, ylab, zlab)
      # scat3d(x[,1],x[,2],x[,3],xlab=xlab, ylab=ylab, zlab=zlab,...)
   } else {
      data <- data.frame(x, y, z)
      names(data) <- c(deparse(substitute(x)),
        deparse(substitute(y)),
        deparse(substitute(z)))

      # scat3d(x,y,z, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), zlab = deparse(substitute(z)), ... )
   }
   fmla <- as.formula( paste( names(data)[2] ,"~", names(data)[1],
        "+", names(data)[3]))
   Plot3d( fmla, data , ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
