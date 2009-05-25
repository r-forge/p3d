Init3d <-
function( par3d = list(),
      family = c('sans','serif', "mono", "symbol"),
      cex = .8,
      font = 1,
      ...) {
    family = match.arg(family)
    mod = list(family=family, cex = cex, font = font)
    par3d [ names(mod)] <- mod
    require(rgl)
    require(mgcv)
    rgl.open()
    do.call('par3d', par3d)
}

