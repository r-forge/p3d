ell <-
function( center, shape , radius  = 1, n =100) {
          fac <- function( x )  {
              # fac(M) is a 'right factor' of a positive semidefinite M
              # i.e. M = t( fac(M) ) %*% fac(M)
              # similar to chol(M) but does not require M to be PD.
              xx <- svd(x)
          t(xx$v) * sqrt(pmax( xx$d,0))
          }
         angles = (0:n) * 2 * pi/n
         if ( length(radius) > 1) {
            ret <- lapply( radius, function(r) rbind(r*cbind( cos(angles), sin(angles)),NA))
            circle <- do.call( rbind, ret)
         }
         else circle = radius * cbind( cos(angles), sin(angles))
         ret <- t( c(center) + t( circle %*% fac(shape)))
         attr(ret,"parms") <- list ( center = rbind( center), shape = shape, radius = radius)
         class(ret) <- "ell"
         ret
    }

