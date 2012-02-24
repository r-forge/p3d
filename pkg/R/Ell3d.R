##
## p3d:Ell3d.R
## 2011-12-22
##

##  Added 'partial ellipse' from p3d.beta 2011-11-05 (GM)



Ell3d <- function(x , ...) {
#   Adds a data ellipse(s) to a 3D plot using the data frame"
#   for the plot"
      UseMethod("Ell3d")
}
Ell3d.default <- function( x,  radius = 1, col,
    alpha = 0.5,
    ellipsoid = TRUE,
    partial = NULL,
    partial.col = "black",
    partial.lwd = 1,
    use.groups = pars$has.groups,
    verbose = 0,...) {
  Rebind <- function( mat, i ) {
          if ( i > ncol(mat)) return( cbind(mat,0))
          if ( i == 1 ) return( cbind(0,mat))
          cbind( mat[, 1:(i-1)], 0, mat[,i:(ncol(mat)-1)])
  }
##
## TO DO: modify to take center and shape
##

  condvar <- function( vv, i ) {
    vv[-i,-i] - 
    vv[-i,i,drop=FALSE]%*%solve( vv[i,i,drop=FALSE],vv[i,-i,drop=FALSE])
  }  
  Levels <- function(x) {
        if (is.factor(x))
            levels(x)
        else unique(x)
  }

  pars <- Plot3d.par()
  if (missing(col)) col <- pars$col
  if( !missing(x) ) {
        if ( is.data.frame(x) ) {
                xmat <- as.matrix(x[,pars$names[c('x','y','z')]] )
        } else {
            xmat <- x
        }
        vv <- var(xmat)
        cc <- apply(xmat,2,mean)
        if (ellipsoid) plot3d( ellipse3d(vv,centre = cc, t = radius),
                col=col, alpha = alpha, add = TRUE)
        if ( !is.null(partial) ){
            for ( jj in partial ){
                  ell.lines <- ell( center = cc[-jj],
                            shape = condvar(vv,jj),
                            radius = radius)
                  ell.lines <- Rebind( ell.lines, jj)
                  ell.lines[,jj] <- cc[jj]
                  Lines3d(ell.lines, col = partial.col, lwd = partial.lwd)
            }
        }
    } else {  # use displayed data
    if (verbose > 1)
      disp(col)
    xmat <- as.matrix(pars$data[,pars$names[c('x','y','z')]] )
    if( !use.groups ) {
      if (nrow( xmat)>1){
        vv <- var(xmat)
        cc <- apply(xmat,2,mean)
        plot3d( ellipse3d(vv,centre = cc, t = radius),
            col=col[1], alpha = alpha, add = TRUE)
        if ( !is.null(partial) ){
          for ( jj in partial ){
                ell.lines <- ell( center = cc[-jj],
                          shape = condvar(vv,jj),
                          radius = radius)
                ell.lines <- Rebind( ell.lines, jj)
                ell.lines[,jj] <- cc[jj]
                Lines3d(ell.lines, col = partial.col, lwd = partial.lwd)
          }
        }
      }
    } else {
      inds <- split( 1:nrow(xmat), pars$data[,pars$names['g']])
      lapply ( seq_along(inds), function( ii ) {
         gmat <- xmat[inds[[ii]],,drop = FALSE]
         if( nrow(gmat) > 1) {
            vv <- var(gmat)
            #disp(vv)
            #disp(gmat)
            ev <- eigen(vv,symmetric=TRUE, only.values = TRUE)
            # fatten up nearly singular matrix to avoid crash in rgl
            if ( (max(ev$values)/min(ev$values)) > 1e07 )
              vv <- vv + (max(ev$values)*1e-07) *diag(nrow(vv))
            cc <- apply(gmat, 2, mean)
            plot3d( ellipse3d(vv,centre = cc, t = radius),
                col=col[ii], alpha=0.5, add = TRUE)
            
               if ( !is.null(partial) ){
        for ( jj in partial ){
              ell.lines <- ell( center = cc[-jj],
                        shape = condvar(vv,jj),
                        radius = radius)
              ell.lines <- Rebind( ell.lines, jj)
              ell.lines[,jj] <- cc[jj]
              Lines3d(ell.lines, col = partial.col, lwd = partial.lwd)
        }
    }
                      
            
         }
      }
      )
    }
}
invisible(0)
}
