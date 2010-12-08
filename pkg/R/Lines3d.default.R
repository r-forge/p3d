Lines3d.default <-
function( ... )  {
    a <- args3d(...)
    # disp(a)
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


# Lines3d( y = 1, z = 1.1, x =c(0,2), col = 'red')
# Lines3d( y = 1.1, z = 1.1, x =c(0,2), color = 'red')

# Lines3d( y = 1, z = 1.2, x =c(0,2))
# Lines3d( y = 1.1, z = 1.2, x =c(0,2))


# Lines3d( y = 1, z = 1.5, x =c(0,2), col = 'red3', lwd = 3)
# Lines3d( y = 1.1, z = 1.5, x =c(0,2), color = 'red3', lwd = 3)
# Lines3d( y = 1.1, z = 1.8, x =c(0,2),col='red',lwd = 3)
# material3d()