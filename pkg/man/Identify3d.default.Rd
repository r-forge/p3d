\name{Identify3d.default}
\Rdversion{1.1}
\alias{Identify3d.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Identify3d.default(x = .Plot3d.par$data[[.Plot3d.par$names["x"]]], y = .Plot3d.par$data[[.Plot3d.par$names["y"]]], z = .Plot3d.par$data[[.Plot3d.par$names["z"]]], labels = rownames(.Plot3d.par$data), groups = NULL, col = c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"), adj = 0, debug = F, pad = 0, offset = ((100/length(x))^(1/3)) * 0.02)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{z}{
%%     ~~Describe \code{z} here~~
}
  \item{labels}{
%%     ~~Describe \code{labels} here~~
}
  \item{groups}{
%%     ~~Describe \code{groups} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{adj}{
%%     ~~Describe \code{adj} here~~
}
  \item{debug}{
%%     ~~Describe \code{debug} here~~
}
  \item{pad}{
%%     ~~Describe \code{pad} here~~
}
  \item{offset}{
%%     ~~Describe \code{offset} here~~
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
function (
    x = .Plot3d.par$data[[.Plot3d.par$names['x']]],
    y = .Plot3d.par$data[[.Plot3d.par$names['y']]],
    z = .Plot3d.par$data[[.Plot3d.par$names['z']]],
    labels = rownames(.Plot3d.par$data), groups = NULL,
    col = c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"),
    adj = 0, debug = F, pad = 0,
    offset = ((100/length(x))^(1/3)) * 0.02) {
    ## added by GM Nov 9
       if ( ( is.matrix(x) && (ncol(x) > 1) ) || is.data.frame(x) ) {
            dat <- x
            x <- dat[,1]
            y <- dat[,2]
            z <- dat[,3]
            labels <- rownames(dat)
       }
    select3d <-function (...) {
    # adapted from select3d and rgl.selecte3d in the rgl package, but passes
    #  through arguments to rgl.select
        rgl:::.check3d()
        rect <- rgl:::rgl.select(...)
        llx <- rect[1]
        lly <- rect[2]
        urx <- rect[3]
        ury <- rect[4]
        if (llx > urx) {
            temp <- llx
            llx <- urx
            urx <- temp
            }
        if (lly > ury) {
            temp <- lly
            lly <- ury
            ury <- temp
            }
        proj <- rgl:::rgl.projection()
        function(x, y, z) {
            pixel <- rgl.user2window(x, y, z, proj = proj)
            apply(pixel, 1, function(p) (llx <= p[1]) && (p[1] <=
                urx) && (lly <= p[2]) && (p[2] <= ury) && (0 <= p[3]) &&
                (p[3] <= 1))
            }
        }
    valid <- if (is.null(groups)) complete.cases(x, y, z)
    else complete.cases(x, y, z, groups)
    x <- x[valid]
    y <- y[valid]
    z <- z[valid]
    labels <- labels[valid]
    pad <- paste( rep(" ", pad), collapse = "")
    labels.pad <- paste(pad,labels)
    # x <- (x - min(x))/(max(x) - min(x))
    # y <- (y - min(y))/(max(y) - min(y))
    # z <- (z - min(z))/(max(z) - min(z))
    rgl.bringtotop()
    identified <- character(0)
    groups <- if (!is.null(groups)) as.numeric(groups[valid])
    else rep(1, length(x))
    repeat {
        f <- select3d(button="right")
        if ( debug ) disp(x)
        which <- f(x, y, z)
        if (debug) disp(which)
        if (!any(which)) break
        rgl.texts(x[which], y[which] + offset, z[which], labels.pad[which],
            color = col[groups][which], adj = adj)
        identified <- c(identified, labels[which])
        }
    identified
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
