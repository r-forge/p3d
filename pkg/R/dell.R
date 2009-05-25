dell <-
function( x, y, radius = 1, ...) {
        if ( (is.matrix(x) && (ncol(x) > 1))|| is.data.frame(x)) mat <- as.matrix(x[,1:2])
        else if (is.list(x)) mat <- cbind(x$x, x$y)
        else mat <- cbind( x,y)
        ell( apply(mat,2,mean), var(mat), radius = radius, ...)
    }

