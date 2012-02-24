##
## p3d:Init3d.R
## 2011-12-22
##
##
##  GM: mod 2011-03-07: added ... to rgl.open to allow:

# From: Carrie Smith:
# Also, I remembered why I used open3d() from rgl instead of Init3d() in the demo in class.
# Init3d() on Mac opens a small window but it can't be resized.
# I was able to specify a larger window using open3d(windowRect=c(x,x,x,x)),
# but I don't think windowRect is a valid argument for Init3d().
# Don't know if that's an easy thing to change, but if it is I think Mac users would appreciate it.

##



Init3d <-
function( par3d = list(),
      family = c('sans','serif', "mono", "symbol"),
      mouseMode = c('polar','fov','zoom'),
      cex = .8,
      font = 1,
      ...) {
    family = match.arg(family)
    mod = list(family=family, cex = cex, font = font)
    par3d [ names(mod)] <- mod
    require(rgl)
    require(mgcv)
    rgl.open(...)
    do.call('par3d', par3d)
}

