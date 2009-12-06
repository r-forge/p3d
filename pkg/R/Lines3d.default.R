Lines3d.default <-
function( ..., color = 'blue' )  {
    a <- args3d(..., color=color)
    xyz <- a$x
    if (nrow(xyz) == 0) return( invisible(0))
    nas <- apply(xyz, 1, function(x) any(is.na(x)))
    inds <- 1:nrow(xyz)
    inds[nas] <- NA
    inds <-  cbind(inds[-length(inds)],inds[-1])
    inds <- na.omit(inds)
    inds <- c(t(inds))
    xyz <- xyz[inds,]
    a$x <- xyz
    # names(a) <- sub('^lwd$','size', names(a))
    # do.call("rgl.lines", a)
    do.call("segments3d", a)
}

