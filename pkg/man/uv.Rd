\name{uv}
\Rdversion{1.1}
\alias{uv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
uv(shape, u, radius = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shape}{
%%     ~~Describe \code{shape} here~~
}
  \item{u}{
%%     ~~Describe \code{u} here~~
}
  \item{radius}{
%%     ~~Describe \code{radius} here~~
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
function( shape, u , radius = 1) {
       # returns 'unit' u and conjugate v
       ConjComp <- function( X , Z = diag( nrow(X)) , ip = diag( nrow(X)), tol = 1e-07 ) {

                     # ConjComp returns a basis for the conjugate complement of the
                     # conjugate projection of X into span(Z) with respect to inner product with
                     # matrix ip.
                     # Note: Z is assumed to be of full column rank but not necessarily X.
                     xq <- qr(t(Z) \%*\% ip \%*\% X, tol = tol)
                     if ( xq$rank == 0 ) return( Z )
                     a <- qr.Q( xq, complete = T ) [ ,-(1:xq$rank)]
                     Z \%*\% a
            }

            u <- u / sqrt( sum( u*solve(shape,u)))   # 'unit' vector in direction of dir
            v <- c(ConjComp( u, diag(2) , solve(shape)))  # conjugate
            v <- v / sqrt( sum( v * solve( shape, v)))
       list(u = radius * u, v= radius * v)
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
