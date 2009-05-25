Plot3d <-
function( x, y, z, ...) {
'
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
        debug = T,
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
        clear = T,
        pow = -1,
        origin = F,
        axes = T,
        lines.col = "gray",
        lines.lwd= 1)
'
    UseMethod("Plot3d")
}

