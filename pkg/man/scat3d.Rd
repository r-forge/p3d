\name{scat3d}
\Rdversion{1.1}
\alias{scat3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scat3d(x, y, z, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), zlab = deparse(substitute(z)), xtend = 0.05, debug = T, xlim = xrange(x, xtend), ylim = xrange(y, xtend), zlim = xrange(z, xtend), revolutions = 0, col = "yellow", bg.col = c("white", "black"), axis.col = if (bg.col == "white") "black" else "white", surface.col = c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"), neg.res.col = "red", pos.res.col = "green", point.col = col, text.col = axis.col, grid.col = if (bg.col == "white") "black" else "gray", fogtype = c("none", "exp2", "linear", "exp"), residuals = (length(fit) == 1), surface = FALSE, fill = TRUE, grid = TRUE, grid.lines = 26, df.smooth = NULL, df.additive = NULL, sphere.size = 1, threshold = 0.01, speed = 1, fov = 30, fit = "linear", groups = NULL, parallel = TRUE, ellipsoid = FALSE, var = FALSE, level = 0.5, model.summary = FALSE, verbose = 0, clear = T, pow = -1, origin = F, axes = TRUE)
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
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{zlab}{
%%     ~~Describe \code{zlab} here~~
}
  \item{xtend}{
%%     ~~Describe \code{xtend} here~~
}
  \item{debug}{
%%     ~~Describe \code{debug} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{zlim}{
%%     ~~Describe \code{zlim} here~~
}
  \item{revolutions}{
%%     ~~Describe \code{revolutions} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{bg.col}{
%%     ~~Describe \code{bg.col} here~~
}
  \item{axis.col}{
%%     ~~Describe \code{axis.col} here~~
}
  \item{surface.col}{
%%     ~~Describe \code{surface.col} here~~
}
  \item{neg.res.col}{
%%     ~~Describe \code{neg.res.col} here~~
}
  \item{pos.res.col}{
%%     ~~Describe \code{pos.res.col} here~~
}
  \item{point.col}{
%%     ~~Describe \code{point.col} here~~
}
  \item{text.col}{
%%     ~~Describe \code{text.col} here~~
}
  \item{grid.col}{
%%     ~~Describe \code{grid.col} here~~
}
  \item{fogtype}{
%%     ~~Describe \code{fogtype} here~~
}
  \item{residuals}{
%%     ~~Describe \code{residuals} here~~
}
  \item{surface}{
%%     ~~Describe \code{surface} here~~
}
  \item{fill}{
%%     ~~Describe \code{fill} here~~
}
  \item{grid}{
%%     ~~Describe \code{grid} here~~
}
  \item{grid.lines}{
%%     ~~Describe \code{grid.lines} here~~
}
  \item{df.smooth}{
%%     ~~Describe \code{df.smooth} here~~
}
  \item{df.additive}{
%%     ~~Describe \code{df.additive} here~~
}
  \item{sphere.size}{
%%     ~~Describe \code{sphere.size} here~~
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
}
  \item{speed}{
%%     ~~Describe \code{speed} here~~
}
  \item{fov}{
%%     ~~Describe \code{fov} here~~
}
  \item{fit}{
%%     ~~Describe \code{fit} here~~
}
  \item{groups}{
%%     ~~Describe \code{groups} here~~
}
  \item{parallel}{
%%     ~~Describe \code{parallel} here~~
}
  \item{ellipsoid}{
%%     ~~Describe \code{ellipsoid} here~~
}
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{level}{
%%     ~~Describe \code{level} here~~
}
  \item{model.summary}{
%%     ~~Describe \code{model.summary} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{clear}{
%%     ~~Describe \code{clear} here~~
}
  \item{pow}{
%%     ~~Describe \code{pow} here~~
}
  \item{origin}{
%%     ~~Describe \code{origin} here~~
}
  \item{axes}{
%%     ~~Describe \code{axes} here~~
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
function(x, y, z, xlab=deparse(substitute(x)), ylab=deparse(substitute(y)),
    zlab=deparse(substitute(z)),
    xtend = .05,
    debug = T,
    xlim = xrange( x, xtend ),
    ylim = xrange( y, xtend ),
    zlim = xrange( z, xtend ),
    revolutions = 0,
    col = "yellow",
    bg.col = c("white", "black"),
    axis.col = if (bg.col == "white") "black" else "white",
    surface.col = c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"),
    neg.res.col = "red",
    pos.res.col = "green",
    point.col = col,
    text.col = axis.col,
    grid.col = if (bg.col == "white") "black" else "gray",
    fogtype = c("none","exp2", "linear", "exp"),
    residuals = (length(fit) == 1),
    surface=FALSE,
    fill=TRUE,
    grid=TRUE,
    grid.lines=26,
    df.smooth=NULL,
    df.additive=NULL,
    sphere.size =1,
    threshold=0.01,
    speed=1,
    fov=30,
    fit="linear",
    groups=NULL,
    parallel=TRUE,
    ellipsoid=FALSE,
    var=FALSE,     # variance ellipsoid
    level=0.5,
    model.summary=FALSE,
    verbose = 0,
    clear = T,
    pow = -1,
    origin = F,
    axes = TRUE) {

    xrange <- function( x, xtend) {
        r <- range(x)
        mean(r) + (1 + xtend) * (r - mean(r))
    }

    require(rgl)
    require(mgcv)
    summaries <- list()
    if ((!is.null(groups)) && (nlevels(groups) > length(surface.col)))
         surface.col <- c(surface.col,rainbow( nlevels(groups) - length(surface.col)))
      #  stop(sprintf("Number of groups (\%d) exceeds number of colors (\%d).",
      #      nlevels(groups), length(surface.col)))
    if ((!is.null(groups)) && (!is.factor(groups))) groups <- as.factor(groups)
    bg.col <- match.arg(bg.col)
    fogtype <- match.arg(fogtype)
    if ((length(fit) > 1) && residuals && surface)
        stop("cannot plot both multiple surfaces and residuals")
    xlab  # cause these arguments to be evaluated
    ylab
    zlab
    if ( verbose > 1 ) disp(zlab)
    if ( verbose > 1 ) disp(clear)
    if ( clear ) {
    	#	rgl.open()
        rgl.clear()
        rgl.viewpoint(fov=fov)
        rgl.bg(col=bg.col, fogtype=fogtype)
        valid <- if (is.null(groups)) complete.cases(x, y, z)
            else complete.cases(x, y, z, groups)
        x <- x[valid]
        y <- y[valid]
        z <- z[valid]
        if (!is.null(groups)) groups <- groups[valid]
        # x <- (x - min(x))/(max(x) - min(x))
        # y <- (y - min(y))/(max(y) - min(y))
        # z <- (z - min(z))/(max(z) - min(z))

        # rgl.lines(c(0,1), c(0,0), c(0,0), color=axis.col)
        # rgl.lines(c(0,0), c(0,1), c(0,0), color=axis.col)
        # rgl.lines(c(0,0), c(0,0), c(0,1), color=axis.col)

        par3d( ignoreExtent = FALSE )

        axx <- xlim
        axy <- ylim
        axz <- zlim

        if( origin[1] != FALSE ) {
             if ( origin[1] == TRUE ) origin <- c('x','y','z')
             if ( "x" \%in\% origin ) axx[1] <- 0
             if ( "y" \%in\% origin ) axy[1] <- 0
             if ( "z" \%in\% origin ) axz[1] <- 0
        }

        rgl.lines( axx[ c(1,2) ], axy[ c(1,1) ] , axz [ c(1,1) ], color = axis.col)  # x
        rgl.lines( axx[ c(1,1) ], axy[ c(1,2) ] , axz [ c(1,1) ], color = axis.col)  # y
        rgl.lines( axx[ c(1,1) ], axy[ c(1,1) ] , axz [ c(1,2) ], color = axis.col)  # z
       
        # rgl.texts(1, 0, 0, xlab, adj=1, color=text.col)
        # rgl.texts(0, 1, 0, ylab, adj=1, color=text.col)
        # rgl.texts(0, 0, 1, zlab, adj=1, color=text.col)


        rgl.texts(axx[2], axy[1], axz[1] , xlab, adj=1, color=text.col)
        rgl.texts(axx[1], axy[2], axz[1] , ylab, adj=1, color=text.col)
        rgl.texts(axx[1], axy[1], axz[2] , zlab, adj=1, color=text.col)
				
				scale <-  1/c( diff(range(c(axx,xlim))), diff(range(c(axy,ylim))), diff(range(c(axz,zlim))))
        scale <- scale / mean(scale)
        par3d( scale =  scale)
				par3d( zoom = 4*min(scale))        # need to improve zoom and fov
        Plot3d.par( abox = c( axx, axy, axz))

        ##
        ## NEED TO IMPROVE THE size ALGORITHM
        ##
        gm <- function(x,pow=1) if(pow !=0) mean( x^pow)^(1/pow) else exp(mean(log(x)))
        # size <- size.sphere*3*((1/mean(1/diff(xlim), 1/diff(ylim), 1/diff(zlim)))^(1/3))*((100/length(x))^(1/3))*0.015
        # size <- size.sphere*((1/mean(1/diff(xlim), 1/diff(ylim), 1/diff(zlim)))^(1))*((100/length(x))^(1/3))*0.015
        # size <- size.sphere*(diff(zlim)^(1))*((100/length(x))^(1/3))*0.015
        # size <- size.sphere*((100/length(x))^(1/3))*0.015
        size <- sphere.size*gm(c(diff(xlim),diff(ylim),diff(zlim)),pow)*((100/length(x))^(1/3))*0.015

        Plot3d.par(size=size)
        if( verbose > 1 ) {
            disp(size)
            disp(xlim)
            disp(axx)
            disp(ylim)
            disp(axy)
            disp(zlim)
            disp(axz)
        }
    } else {  # if ! clear
        size <- Plot3d.par('size')
    } # end clear

    if (is.null(groups)){
        if (size > threshold) rgl.spheres(x, y, z, color=point.col, radius=size)
            else rgl.points(x, y, z, color=point.col)
            }
    else {
        if (size > threshold) rgl.spheres(x, y, z,
            color=surface.col[as.numeric(groups)], radius=size)
        else rgl.points(x, y, z, color=surface.col[as.numeric(groups)])
        }

    par3d(ignoreExtent = TRUE)
    if (ellipsoid || var) {
        dfn <- 3
        if (is.null(groups)){
            dfd <- length(x) - 1
            radius <- sqrt(dfn * qf(level, dfn, dfd))
            if ( var ) radius <- 1
            ellips <- ellipsoid(center=c(mean(x), mean(y), mean(z)),
                shape=cov(cbind(x,y,z)), radius=radius)
            if (fill) shade3d(ellips, col=surface.col[1], alpha=0.1, lit=FALSE)
            if (grid) wire3d(ellips, col=surface.col[1], lit=FALSE)
            }
        else{
            levs <- levels(groups)
            for (j in 1:length(levs)){
                group <- levs[j]
                select.obs <- groups == group
                xx <- x[select.obs]
                yy <- y[select.obs]
                zz <- z[select.obs]
                dfd <- length(xx) - 1
                radius <- sqrt(dfn * qf(level, dfn, dfd))
                if (var) radius <- 1
                ellips <- ellipsoid(center=c(mean(xx), mean(yy), mean(zz)),
                    shape=cov(cbind(xx,yy,zz)), radius=radius)
                if (fill) shade3d(ellips, col=surface.col[j], alpha=0.1, lit=FALSE)
                if (grid) wire3d(ellips, col=surface.col[j], lit=FALSE)
                coords <- ellips$vb[, which.max(ellips$vb[1,])]
                if (!surface) rgl.texts(coords[1] + 0.05, coords[2], coords[3], group,
                    col=surface.col[j])
                }
            }
        }

    if (surface){
        vals.x <- seq(xlim[1], xlim[2], length=grid.lines)
        vals.z <- seq(zlim[1], zlim[2], length=grid.lines)
        dat <- expand.grid(x=vals.x, z=vals.z)
        for (i in 1:length(fit)){
            f <- match.arg(fit[i], c("linear", "quadratic", "smooth", "additive"))
            if (is.null(groups)){
                mod <- switch(f,
                    linear = lm(y ~ x + z),
                    quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2)),
                    smooth = if (is.null(df.smooth)) gam(y ~ s(x, z))
                        else gam(y ~ s(x, z, fx=TRUE, k=df.smooth)),
                    additive = if (is.null(df.additive)) gam(y ~ s(x) + s(z))
                        else gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) +
                            s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1)))
                    )
                if (model.summary) summaries[[f]] <- summary(mod)
                yhat <- matrix(predict(mod, newdata=dat), grid.lines, grid.lines)
                if (fill) rgl.surface(vals.x, vals.z, yhat, color=surface.col[i],
                    alpha=0.5, lit=FALSE)
                if(grid) rgl.surface(vals.x, vals.z, yhat, color=if (fill) grid.col
                    else surface.col[i], alpha=0.5, lit=FALSE, front="lines", back="lines")
                if (residuals){
                    n <- length(y)
                    fitted <- fitted(mod)
                    colors <- ifelse(residuals(mod) > 0, pos.res.col, neg.res.col)
                    rgl.lines(as.vector(rbind(x,x)), as.vector(rbind(y,fitted)),
                        as.vector(rbind(z,z)), color=as.vector(rbind(colors,colors)))
                    }
                }
            else{
                if (parallel){
                    mod <- switch(f,
                        linear = lm(y ~ x + z + groups),
                        quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2) + groups),
                        smooth = if (is.null(df.smooth)) gam(y ~ s(x, z) + groups)
                            else gam(y ~ s(x, z, fx=TRUE, k=df.smooth) + groups),
                        additive = if (is.null(df.additive)) gam(y ~ s(x) + s(z) + groups)
                            else gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) +
                                s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1)) + groups)
                        )
                    if (model.summary) summaries[[f]] <- summary(mod)
                    levs <- levels(groups)
                    for (j in 1:length(levs)){
                        group <- levs[j]
                        select.obs <- groups == group
                        yhat <- matrix(predict(mod, newdata=cbind(dat, groups=group)),
                            grid.lines, grid.lines)
                        if (fill) rgl.surface(vals.x, vals.z, yhat, color=surface.col[j],
                            alpha=0.5, lit=FALSE)
                        if (grid) rgl.surface(vals.x, vals.z, yhat, color=if (fill) grid.col
                            else surface.col[j], alpha=0.5, lit=FALSE,
                                front="lines", back="lines")
                        rgl.texts(0, predict(mod, newdata=data.frame(x=0, z=0,
                            groups=group)), 0,
                            paste(group, " "), adj=1, color=surface.col[j])
                        if (residuals){
                            yy <- y[select.obs]
                            xx <- x[select.obs]
                            zz <- z[select.obs]
                            fitted <- fitted(mod)[select.obs]
                            rgl.lines(as.vector(rbind(xx,xx)),
                                as.vector(rbind(yy,fitted)), as.vector(rbind(zz,zz)),
                                col=surface.col[j])
                            }
                        }
                    }
                else {
                    levs <- levels(groups)
                    for (j in 1:length(levs)){
                        group <- levs[j]
                        select.obs <- groups == group
                        mod <- switch(f,
                            linear = lm(y ~ x + z, subset=select.obs),
                            quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2),
                                    subset=select.obs),
                            smooth = if (is.null(df.smooth)) gam(y ~ s(x, z),
                                    subset=select.obs)
                                else gam(y ~ s(x, z, fx=TRUE, k=df.smooth),
                                    subset=select.obs),
                            additive = if (is.null(df.additive)) gam(y ~ s(x) + s(z),
                                    subset=select.obs)
                                else gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) +
                                    s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1)),
                                    subset=select.obs)
                            )
                        if (model.summary)
                            summaries[[paste(f, ".", group, sep="")]] <- summary(mod)
                        yhat <- matrix(predict(mod, newdata=dat), grid.lines, grid.lines)
                        if (fill) rgl.surface(vals.x, vals.z, yhat, color=surface.col[j],
                            alpha=0.5, lit=FALSE)
                        if (grid) rgl.surface(vals.x, vals.z, yhat, color=if (fill) grid.col
                            else surface.col[j], alpha=0.5, lit=FALSE,
                                front="lines", back="lines")
                        rgl.texts(0, predict(mod,
                            newdata=data.frame(x=0, z=0, groups=group)), 0,
                            paste(group, " "), adj=1, color=surface.col[j])
                        if (residuals){
                            yy <- y[select.obs]
                            xx <- x[select.obs]
                            zz <- z[select.obs]
                            fitted <- fitted(mod)
                            rgl.lines(as.vector(rbind(xx,xx)), as.vector(rbind(yy,fitted)),
                                as.vector(rbind(zz,zz)),
                                col=surface.col[j])
                            }
                        }
                    }
                }
            }
        }
    if (revolutions > 0) {
        for (i in 1:revolutions){
            for (angle in seq(1, 360, length=360/speed)) rgl.viewpoint(-angle, fov=fov)
            }
        }
    if (model.summary) return(summaries) else return(invisible(NULL))
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
