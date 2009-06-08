\name{Id3d}
\Rdversion{1.1}
\alias{Id3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify points in the current Plot3d rgl window
}
\description{
Drag a rectangle around a point to identify it, with the point label
as specified in the \code{labels} argument.

Drag a rectangle around an empty region to exit from point identification
mode, or make the R console the active window and press \code{<ESC>}.
}
\usage{
Id3d(labels = row.names(data), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labels}{
Observation labels
}
  \item{\dots}{
Other arguments, passed down
}
}
\details{
By default, observations are labeled using the \code{row.names} in the 
data frame for the current \code{Plot3d} window.
}
\value{
None}
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
\code{\link{Identify3d}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(labels=row.names(data),...) {
    pars <- .Plot3d.par
    data <- pars$data
    nams <- pars$names
    if ( pars$has.groups ) {
        Identify3d(
            data[[nams['x']]],  data[[nams['y']]],data[[nams['z']]],
            groups = data[[nams['g']]],labels=labels,...)
    } else {
        Identify3d(
            data[[nams['x']]],  data[[nams['y']]],data[[nams['z']]],
            labels=labels,...)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
