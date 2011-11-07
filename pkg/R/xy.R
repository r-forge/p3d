# fixed buglet using plist in do.call
xy <-
function( p1, p2 ) {
          # turn list of points into matrix for plotting
          if( is.list( p1) ) do.call( rbind, p1)
          else rbind( p1, p2)
    }

