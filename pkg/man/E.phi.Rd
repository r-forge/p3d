\name{E.phi}
\Rdversion{1.1}
\alias{E.phi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Incline horizontal plane by phi degree
}
\description{
Incline horizontal plane by phi degree
}
\usage{
E.phi(phi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phi}{
     Angle
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
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Georges Monette
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( phi ) {
    # incline horizontal plane by phi degrees
    # note that userMatrix for spinning has form E.phi \%*\% S.theta
    phi = phi *2*pi/360
    cbind(c(1,0,0,0), c(0,cos(phi),sin(phi),0),  c(0,-sin(phi), cos(phi),0), c(0,0,0,1))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
