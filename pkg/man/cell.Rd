\name{cell}
\Rdversion{1.1}
\alias{cell}
\alias{cell.default}
\alias{cell.glh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate confidence ellipses for parameters in linear models and linear hypotheses
}
\description{
\code{cell} is similar to \code{confidence.ellipse} in the \code{car}
package,
except that \code{cell} returns the points to form the ellipse(s)
which must be plotted with \code{plot(...,type='l')} or \code{lines(...)}.
It currently has methods for \code{lm} (\code{cell.default}) and \code{glh} objects.
}
\usage{
cell(object, ...)

\method{cell}{default}{object, which.coef, levels = 0.95, Scheffe = FALSE, dfn = 2, center.pch = 19, 
center.cex = 1.5, segments = 51, xlab, ylab, las = par("las"), col = palette()[2], lwd = 2, lty = 1, 
add = FALSE, ...}

\method{cell}{glh}{object, which.coef = 1:2, levels = 0.95, Scheffe = FALSE, dfn = 2, center.pch = 19, 
center.cex = 1.5, segments = 51, xlab, ylab, las = par("las"), col = palette()[2], lwd = 2, 
lty = 1, add = FALSE, ...}
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{object}{A \code{lm} or \code{glh} object}
   
  \item{which.coef}{
Indices of the coefficients, from \code{coef(object)} for which the confidence ellipse
is to be constructed.
}
  \item{levels}{
One or more values of the confidence coverage of the ellipse(s).
}
  \item{Scheffe}{
%%     ~~Describe \code{Scheffe} here~~
}
  \item{dfn}{
The Scheffe dimension used to calculate the radii of the ellipse(s).
i.e., use  \code{dfn = 1} to generate ellipses whose shadows are
ordinary confidence intervals for individual parameters; use
\code{dfn = 2} for 2-dimensional ellipses, etc.
}
  \item{center.pch}{
Plotting character used to mark the center of the ellipse(s)
}
  \item{center.cex}{
Size of the plotting character
}
  \item{segments}{Number of line segments used to generate the ellipse(s)}
 
  \item{xlab}{
Label for the horizontal coefficient of the plot.
}
  \item{ylab}{
Label for the vertical coefficient of the plot.

}
  \item{las}{
Style of axis labels
}
  \item{col}{
Color for the ellipse(s)
}
  \item{lwd}{
Line width for the ellipse(s)
}
  \item{lty}{
Line type for the ellipse(s)
}
  \item{add}{
This argument is not presently used
}
  \item{\dots}{
Other arguments passed down [where?]
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns an object of class \code{ell} representing points on the boundaries of
the confidence ellipse(s).
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
\code{\link{ell}}, for details on \code{ell} objects

}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( ... )  {
            UseMethod("cell")
            help <- "
            See help for car::confidence.ellipse.lm
            except that 'cell' returns the points to form the ellipse
            which must be plotted with plot(...,type='l') or lines(...)
            -- Use dfn to determine Sheffe dimension, i.e. dfn = 1 to generate ordinary CIs, dfn = 2 for 2-dim CE, etc.
            -- TODO: extend to 3 dimensions if which.coef has length 3
            "
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
\keyword{ aplot }% __ONLY ONE__ keyword per line
