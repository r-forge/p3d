uv <-
function( shape, u , radius = 1) {
       # returns 'unit' u and conjugate v
       ConjComp <- function( X , Z = diag( nrow(X)) , ip = diag( nrow(X)), tol = 1e-07 ) {

                     # ConjComp returns a basis for the conjugate complement of the
                     # conjugate projection of X into span(Z) with respect to inner product with
                     # matrix ip.
                     # Note: Z is assumed to be of full column rank but not necessarily X.
                     xq <- qr(t(Z) %*% ip %*% X, tol = tol)
                     if ( xq$rank == 0 ) return( Z )
                     a <- qr.Q( xq, complete = TRUE ) [ ,-(1:xq$rank)]
                     Z %*% a
            }

            u <- u / sqrt( sum( u*solve(shape,u)))   # 'unit' vector in direction of dir
            v <- c(ConjComp( u, diag(2) , solve(shape)))  # conjugate
            v <- v / sqrt( sum( v * solve( shape, v)))
       list(u = radius * u, v= radius * v)
    }

