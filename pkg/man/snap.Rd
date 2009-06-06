\name{snap}
\Rdversion{1.1}
\alias{snap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Take a snapshot of the current 3D plot
}
\description{
\code{snap} is designed as a more convenient interface to \code{rgl.snapshot},
allowing one to set a base filename for graphic output files, an initial
sequence number, and then just use

\code{snap()}

to capture the current screen image to a sequentially numbered output file.
}
\usage{
snap(suffix = "", fn = .snap.fn, n = .snap.n, inc = TRUE, fmt = "png")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{suffix}{
%%     ~~Describe \code{suffix} here~~
}
  \item{fn}{
Base file name, typically set by assigning a character string to \code{.snap.fn}
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{inc}{
Should the function increment the sequential number in the global environment?
}
  \item{fmt}{
Output graphic file format. Default: \code{png}
}
}
\details{
Output files are written to the current directory.  Use \code{setwd} to put them where
you want.
}
\value{
%Returns invisibly the name of the snapshot file
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Georges Monette
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%\code{\link{[rgl]{rgl.snapshot}}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( suffix = "", fn = .snap.fn, n = .snap.n, inc = TRUE, fmt = "png") {
         help <- "
         
         > .snap.fn <- 'Lecture1'
         > .snap.n <- '001'
         > snap()
         
         will invoke rgl.snapshot which filename 'Lecture1001.png' and increment
         .snap.n to '002' in the .GlobalEnv
         "
         
         filename <- paste( fn, n, suffix, ".", fmt , sep = "")
         rgl.snapshot( filename, fmt = fmt)
         if (inc) assign( ".snap.n", Inc(.snap.n), envir = .GlobalEnv)
         invisible( filename )
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ iplot }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
