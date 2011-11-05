\name{Ell3d}
\Rdversion{1.1}
\alias{Ell3d}
\alias{Ell3d.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add ellipsoid(s) to a 3D plot
}
\description{
Adds data ellipsoids(s) to a 3D plot using, by default, the data frame
for the plot.
%So far, Ell3d() will only draw data ellipsoids around displayed data.
\bold{Note:} The argument list of Ell3d has changed.  This documentation
should be regenerated and revised.
}
\usage{
Ell3d(x, ...)
\method{Ell3d}{default}(x, radius = 1, col, alpha = 0.5, use.groups = pars$has.groups, 
    verbose = 0, ...) 

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    optional matrix or data frame used to generate a data ellipsoid.
%%     ~~Describe \code{center} here~~
}
  \item{col}{
    color for ellipsoid, by default: Plot3d.par()$col.
%%     ~~Describe \code{shape} here~~
}
  \item{radius}{  of the ellipsoid (1 generates the standard ellipsoid whose
  one dimensional projections yield the mean plus or minus one standard deviation
  of the variable projected.
}
  \item{use.groups}{ defaults to Plot3d.par()has.groups: whether to plot one ellipsoid for
  all the data or separate ellipsoids for each subgroup.
}
  \item{alpha}{ default 0.5. Controls transparency of ellipsoid.}
  \item{verbose}{be verbose?}
  \item{\dots}{other arguments passed down}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
None
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Georges Monette
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Plot3d}}, ~~~
}
\examples{
\dontrun{
library(p3d)
library(car)

Init3d()
Plot3d( women ~ education + prestige | type, Prestige)
Ell3d()
Ell3d( subset(Prestige, income > 5000), col = 'pink', alpha = .3)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
