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
            lines.lwd= 1,
            ...) {
            
# BUG: subset and groups might not work together

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
        Plot3d.par( data = dd, names=nams, has.groups = FALSE, col=col)
        if ( verbose > 1 ) disp( Plot3d.par() )
        scat3d( dd[[2]], dd[[1]], dd[[3]],
            xlab , ylab, zlab,
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
        Plot3d.par( data = dd, names=nams, has.groups = TRUE, col=col)
        if ( verbose > 1 ) disp( Plot3d.par() )
        scat3d( dd[[2]], dd[[1]], dd[[3]],
            xlab, ylab, zlab, groups = dd[[4]], 
            surface = surface,
            fit = fit,
            surface.col = col, ...)
        if ( verbose >= 0 ) {
            pp <- Plot3d.par()
            cats <- data.frame(x=Levels(pp$data[[pp$names['g']]]))
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
    
}

