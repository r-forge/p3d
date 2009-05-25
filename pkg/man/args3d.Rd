\name{args3d}
\Rdversion{1.1}
\alias{args3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
args3d(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function(...) {
help <- "

    - returns a list with a matrix, x with xyz values and with other args
    - can specify 'min', 'max', 'mid' as values not mixed with numerical
      values
    CAUTION: If first argument is positional then variables
             must be in order x,y,z
"
    na2f <- function( x ) {
         x[is.na(x)] <- FALSE
         x
    }
    args <- list(...)
    
    getdim <- function(nam, args){
        pos <- regexpr(nam, names(args)) 
        arg.ind <- pos > 0
        if( length(pos <- pos [ pos > 0 ])==0) return(0)
        cbind( args[arg.ind][[1]])[,pos]
    }
    
    if( is.null(names(args))) names(args) <- rep("",length(args) )
    nn <- names(args)
		#disp(nn)
        if ( (length(nn) >2) && all( nn[1:3] =="")) names(args) [1:3] <- c('x','y','z')
        nn <- names(args)
        if ( (length(nn) > 0) && (nn[1] == "") ){
						 names(args)[1] <- 'xyz'
						 args[[1]] = rbind( args[[1]])	
				}
        nn <- names(args)
        #disp(nn)
    nxyz <- sapply( nn, regexpr, "xyzyxzxzyyzxzxyzyz")
    #disp(nxyz)

    xyzs <- args[nxyz > 0]
    #disp(xyzs)
    oargs <- args[nxyz < 0]
    #disp(oargs)
		xyz <- cbind( x=getdim('x', xyzs), y=getdim('y',xyzs), z=getdim('z',xyzs))
    if ( is.character(xyz) ) {
        bbox <- par3d('bbox')
        mins <- matrix(bbox[c(1,3,5)], nrow = nrow(xyz), ncol=3, byrow = T)
        maxs <- matrix(bbox[1+c(1,3,5)], nrow = nrow(xyz), ncol=3, byrow = T)
        mids <- (mins + maxs)/2
        xyz[na2f(xyz=='min')] <- mins[na2f(xyz=='min')]
        xyz[na2f(xyz=='max')] <- maxs[na2f(xyz=='max')]
        xyz[na2f(xyz=='mid')] <- mids[na2f(xyz=='mid')]
        mode(xyz) <- "numeric"
    }
    a <- list(x = xyz)
    c(a,oargs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
