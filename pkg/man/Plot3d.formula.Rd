\name{Plot3d.formula}
\Rdversion{1.1}
\alias{Plot3d.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Plot3d.formula(formula = attr(data, "formula"), data = sys.parent(), groups = NULL, subset = NULL, xlab = names(dd)[2], ylab = names(dd)[1], zlab = names(dd)[3], verbose = 0, col = c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"), surface = FALSE, fit = "smooth", surface.col = col, lines.col = "gray", lines.lwd = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{groups}{
%%     ~~Describe \code{groups} here~~
}
  \item{subset}{
%%     ~~Describe \code{subset} here~~
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
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{surface}{
%%     ~~Describe \code{surface} here~~
}
  \item{fit}{
%%     ~~Describe \code{fit} here~~
}
  \item{surface.col}{
%%     ~~Describe \code{surface.col} here~~
}
  \item{lines.col}{
%%     ~~Describe \code{lines.col} here~~
}
  \item{lines.lwd}{
%%     ~~Describe \code{lines.lwd} here~~
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
function( formula = attr(data, "formula"),
            data = sys.parent() ,
            groups = NULL,
            subset = NULL ,
            xlab = names(dd)[2],
            ylab = names(dd)[1], zlab = names(dd)[3],  verbose = 0,
            col = c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"),
            surface = FALSE,
            fit = "smooth",
            surface.col = col,
            lines.col = 'gray',
            lines.lwd= 1,
            ...) {
            
# BUG: subset and groups might not work together

    Levels <- function( x ) if (is.factor(x)) levels(x) else unique(x)
    env <- environment(formula)
    subset <- eval( substitute(subset), data, env)
    groups <- eval( substitute(groups), data, env)
    if (verbose > 0) disp(subset)
    ff <- as.character( formula )
    if ( verbose ) disp( ff )
    if ( length(ff) != 3) stop("Formula should have form y ~ x + z [ |g ]")
    ff[3] <- sub("\\|","+",ff[3])
    if ( verbose ) disp( ff )
    fmla <- as.formula( paste( ff[2] ,"~", ff[3]))
    dd <- model.frame( fmla, data, subset = subset )
    if ( ncol(dd) < 3) stop( "Need at least three variables for Plot3d")
    if ( ncol(dd) > 4) stop( "More than 4 variables not yet implemented")
    if ( ncol(dd) == 3) {
        nams <- names(dd)
        names(nams) <- c('y','x','z')
        Plot3d.par( data = dd, names=nams, has.groups = F, col=col)
        if ( verbose > 1 ) disp( Plot3d.par() )
        scat3d( dd[[2]], dd[[1]], dd[[3]],
            xlab , ylab, zlab,
            surface = surface,
            fit = fit,
            surface.col = surface.col,
            ...)

    }
    if ( ncol(dd)  == 4) {
        #if ( any ( isfac <- sapply( dd[,2:3], is.factor)) && !(is.factor( dd[,4]))) {
        #   consider putting categorical in last place
        #}
        if( verbose) disp(ncol(dd))
        nams <- names(dd)
        names(nams) <- c('y','x','z','g')
        Plot3d.par( data = dd, names=nams, has.groups = T, col=col)
        if ( verbose > 1 ) disp( Plot3d.par() )
        scat3d( dd[[2]], dd[[1]], dd[[3]],
            xlab, ylab, zlab, groups = dd[[4]], 
            surface = surface,
            fit = fit,
            surface.col = col, ...)
        if ( verbose >= 0 ) {
            pp <- Plot3d.par()
            cats <- data.frame(x=Levels(pp$data[[pp$names['g']]]))
            cats$col <- col[1:length(cats$x)]
            names(cats)[1] <- pp$names['g']
            print(cats)
        }
    }
    if( !is.null(groups)) {
      lapply( split( 1:nrow(data), groups), function( ind ) {
              dz <- data[ ind,]
               fmla <- as.formula(paste( sub("\\|","+",as.character(fmla))[c(2,1,3)],collapse =""))
               Lines3d( yxz = model.frame( fmla, dz)[,1:3],col = lines.col,
                  lwd = lines.lwd)
         })
    }
    
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
