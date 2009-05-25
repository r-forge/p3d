
Ell3d <- function(x , ...) {
"   Adds a data ellipse(s) to a 3D plot using the data frame"
"   for the plot"
      UseMethod("Ell3d")
}
Ell3d.default <- function( x,  radius = 1, col,
              alpha = 0.5,  use.groups = pars$has.groups,
              verbose = 0,...) {
    Levels <- function(x) if (is.factor(x))
        levels(x)
    else unique(x)
    if (missing(x)) {  # use displayed data
    pars <- .Plot3d.par
    if (missing(col))
        col <- pars$col
    if (verbose > 1)
        disp(col)
    xmat <- as.matrix(pars$data[,pars$names[c('x','y','z')]] )
    if( !use.groups ) {
        vv <- var(xmat)
        cc <- apply(xmat,2,mean)
        plot3d( ellipse3d(vv,centre = cc, t = radius),
                col=col[1], alpha = alpha, add = TRUE)
    } else {
       inds <- split( 1:nrow(xmat), pars$data[,pars$names['g']])
       lapply ( seq_along(inds), function( ii ) {
             gmat <- xmat[inds[[ii]],]
             vv <- var(gmat)
             cc <- apply(gmat, 2, mean)
             plot3d( ellipse3d(vv,centre = cc, t = radius),
                col=col[ii], alpha=0.5, add = TRUE)
       } )

    }
    }
    invisible(0)
}
