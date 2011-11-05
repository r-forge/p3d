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
Character string appended to \code{paste(fn, n)}, for use in naming a 
related collection of figures (without incrementing \code{n}.
}
  \item{fn}{
Base file name, typically set by assigning a character string to \code{.snap.fn}
in the global environment.
}
  \item{n}{
Initial suffix number used to construct the file name, 
typically set by assigning a character string to \code{.snap.n}
in the global environment.
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
Returns invisibly the name of the snapshot file
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Georges Monette
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[rgl]{rgl.snapshot}}
}
\examples{
\dontrun{
.snap.fn <- 'Lecture1'
.snap.n <- '001'

# ... generate an rgl graph ...
snap()
# ... will invoke rgl.snapshot which filename 'Lecture1001.png' and increment
# .snap.n to '002' in the .GlobalEnv

# ... generate an rgl graph ...
snap()
# ... will invoke rgl.snapshot which filename 'Lecture1002.png' and increment
# .snap.n to '003' in the .GlobalEnv
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ iplot }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
