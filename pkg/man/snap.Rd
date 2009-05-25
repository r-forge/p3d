\name{snap}
\Rdversion{1.1}
\alias{snap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
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
%%     ~~Describe \code{fn} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{inc}{
%%     ~~Describe \code{inc} here~~
}
  \item{fmt}{
%%     ~~Describe \code{fmt} here~~
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
