##
## p3d:Plot3d.R
## 2011-12-22
##

Plot3d <-
function( x, y, z, ..., help = FALSE) {
if (help ) cat('
    Help summary:
    Plot3d ( y ~ x + z [ |g ], data, groups = id,
        sphere.size = 1,
        xlim,   # across screen
        ylim,   # vertical
        zlim,   # into screen
        xlab,
        ylab,
        zlab,
        xtend = .05, # axes extend beyond range of data
        debug = TRUE,
        revolutions = 0,
        bg.col = c("white", "black"),
        axis.col = if (bg.col == "white") "black" else "white",
        surface.col = c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"),
        neg.res.col = "red",
        pos.res.col = "green",
        point.col = "yellow",
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
        threshold=0.01,
        speed=1,
        fov=30,
        fit="linear",
        groups=NULL,
        parallel=TRUE,
        ellipsoid=FALSE,  # data ellipsoid within
        level=0.5,
        model.summary=FALSE,
        verbose = 0,
        clear = TRUE,
        pow = -1,
        origin = TRUE,
        axes = TRUE,
        lines.col = "gray",
        lines.lwd= 1)
') else UseMethod("Plot3d")
}



Plot3d.par <- function(..., new = FALSE){
   # A new version (2010-11-30) of Plot3d.par
   # that maintains separate lists for each rgl window allowing
   # the selection of different windows

  a <- list(...)
  
  #disp( environment())
  #if ( is.null(names(a))) disp(a)
  #else disp(names(a))

  pos <- rgl.cur()

  if ( !exists(".Plot3d.par",1) || pos ==0) assign(".Plot3d.par",list(),1)  # initialize
  if( pos == 0) pos <- 1
  if ( new ){
    p <- get(".Plot3d.par",1)
    p[[pos]] <- list()
    assign(".Plot3d.par",p,1)
  } 
  if ( length( p <- get(".Plot3d.par",1)) < pos){
     p <- get(".Plot3d.par",1)
    p[[pos]] <- list()
    assign(".Plot3d.par",p,1)
  } #.Plot3d.par[[pos]] <<- list()
  if ( length(a) == 0) return(get(".Plot3d.par",1)[[pos]])
  if ( !is.null(names(a))) {
      p <- get(".Plot3d.par",1)
      p[[pos]][names(a)] <- a
      assign(".Plot3d.par",p,1)
    
      ret <- p[[pos]]
   }
   else {
  ret <- get(".Plot3d.par",1)[[pos]][[unlist(a)]]
   }
 ret
 }




Plot3d.lm <-
function( fit , ...) {
    Plot3d(formula(fit), model.frame(fit), ...)
    Fit3d( fit)
}

## This is the S3 method that is used for plotting, i.e. it is called by Plot3d.default
Plot3d.formula <-
function( formula = attr(data, "formula"),
            data = sys.parent() ,
            groups = NULL,
            subset = NULL ,
            xlab = names(dd)[2],
            ylab = names(dd)[1], zlab = names(dd)[3],  verbose = 0,
            col = c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"),
            surface = FALSE,
            fit = "smooth",
            surface.col = col,
            lines.col = 'gray',
            lines.lwd = 1,
            theta = 0,       # arguments for view3d
            phi = 15,
            zoom = 1,
            fov = 15,
#            keep.view = missing(phi) && missing(theta) && missing(zoom) && missing(fov),
            keep.view = FALSE,
            ...) {

# BUG: subset and groups might not work together

    par3d()  # seems more reliable if called first??
    p <- par3d()
    Levels <- function( x ) if (is.factor(x)) levels(x) else unique(x)
    env <- environment(formula)
    subset <- eval( substitute(subset), data, env)
    groups <- eval( substitute(groups), data, env)
    if (verbose > 0) disp(subset)
    ff <- as.character( formula )
    if ( verbose ) disp( ff )
    if ( length(ff) != 3) stop("Formula should have form y ~ x + z [ |g ]")
    ff[3] <- sub("\\|","+",ff[3])
    if ( verbose ) disp( ff )
    fmla <- as.formula( paste( ff[2] ,"~", ff[3]))
    dd <- model.frame( fmla, data, subset = subset )
    if ( ncol(dd) < 3) stop( "Need at least three variables for Plot3d")
    if ( ncol(dd) > 4) stop( "More than 4 variables not yet implemented")
    if ( ncol(dd) == 3) {
        nams <- names(dd)
        names(nams) <- c('y','x','z')
        Plot3d.par( data = dd, names=nams, has.groups = FALSE, col=col, new = TRUE)
        if ( verbose > 1 ) disp( Plot3d.par() )
        scat3d( dd[[2]], dd[[1]], dd[[3]],
            xlab , ylab, zlab,
            col = col,
            surface = surface,
            fit = fit,
            surface.col = surface.col,
            ...)

    }
    if ( ncol(dd)  == 4) {
        #if ( any ( isfac <- sapply( dd[,2:3], is.factor)) && !(is.factor( dd[,4]))) {
        #   consider putting categorical in last place
        #}
        if( verbose) disp(ncol(dd))
        nams <- names(dd)
        names(nams) <- c('y','x','z','g')
        Plot3d.par( data = dd, names=nams, has.groups = TRUE, col=col, new = TRUE)
        if ( verbose > 1 ) disp( Plot3d.par() )
        scat3d( dd[[2]], dd[[1]], dd[[3]],
            xlab, ylab, zlab, groups = dd[[4]],
            surface = surface,
            fit = fit,
            surface.col = col, ...)
        if ( verbose > -1 ) {
            pp <- Plot3d.par()
            cats <- data.frame(x=Levels(pp$data[[pp$names['g']]]))
            #disp(cats)
            #disp(col)
            cats$col <- col[1:length(cats$x)]
            names(cats)[1] <- pp$names['g']
            print(cats)
        }
    }
    if( !is.null(groups)) {
      lapply( split( 1:nrow(data), groups), function( ind ) {
              dz <- data[ ind,]
               fmla <- as.formula(paste( sub("\\|","+",as.character(fmla))[c(2,1,3)],collapse =""))
               Lines3d( yxz = model.frame( fmla, dz)[,1:3],col = lines.col,
                  lwd = lines.lwd)
         })
    }
    if ( verbose > 0 ) disp( 'Plot3d -1')
    if ( verbose > 0 )  disp( par3d('scale'))
    if (keep.view) {
      Acos <- function(x) 360*acos(x)/(2*pi)
      Asin <- function(x) 360*asin(x)/(2*pi)
      Atan2 <- function( s, c) atan2( s, c)*(180/pi)
      um <- p$userMatrix

      theta <- Atan2(-um[1,3], um[1,1])
      phi <-  Atan2 (um[3,2],um[2,2])
      view3d(theta = theta, phi = phi, fov = p$FOV, zoom = p$zoom,
        scale = p$scale)
    }   else {
    view3d( theta = theta, phi = phi, fov = fov, zoom = zoom )
    if ( verbose > 0 )   disp( 'Plot3d in else')
    if ( verbose > 0 ) disp( par3d('scale'))
    }
    cat("\nUse left mouse to rotate, middle mouse (or scroll) to zoom, right mouse to change perspective\n")
}

Plot3d.default <-
function(x, y, z, xlab, ylab, zlab, groups = NULL, ...) {
   help = "
   Plot3d()   by GM Nov 9, 2005
     Modified Feb 7, 2007
   A wrapper for scat3d modified from scatter3d by John Fox
   "
   ####  NOTE: This function needs considerable work
   if ( ! is.null(groups)) stop("groups implemented only in Plot3d.formula")
   if ( is.matrix(x) || is.data.frame(x)) {
      if( ncol (x) < 3) x <- cbind( x , 0, 0)
      if ( is.null( colnames( x ))) {
          colnames(x) <- paste( deparse(substitute(x)), 1:ncol(x))
      }
      if(missing(xlab)) xlab = colnames(x)[1]
      if(missing(ylab)) ylab = colnames(x)[2]
      if(missing(zlab)) zlab = colnames(x)[3]
      dd <- x[,1:3]
      names(dd) <- c(xlab,ylab,zlab)
      data <- data.frame( x[,1], x[,2], x[,3])
      names( data ) <- c(xlab, ylab, zlab)
      # scat3d(x[,1],x[,2],x[,3],xlab=xlab, ylab=ylab, zlab=zlab,...)
   } else {
      data <- data.frame(x, y, z)
      names(data) <- c(deparse(substitute(x)),
        deparse(substitute(y)),
        deparse(substitute(z)))

      # scat3d(x,y,z, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), zlab = deparse(substitute(z)), ... )
   }
   fmla <- as.formula( paste( "`",names(data)[2],"` ~ `", names(data)[1],
        "` + `", names(data)[3], "`", sep = ''))
   Plot3d( fmla, data , ...)
}



