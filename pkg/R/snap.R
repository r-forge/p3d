snap <-
function( suffix = "", fn = .snap.fn, n = .snap.n, inc = TRUE, fmt = "png") {
         help <- "
         
         > .snap.fn <- 'Lecture1'
         > .snap.n <- '001'
         > snap()
         
         will invoke rgl.snapshot which filename 'Lecture1001.png' and increment
         .snap.n to '002' in the .GlobalEnv
         "
         
         filename <- paste( fn, n, suffix, ".", fmt , sep = "")
         rgl.snapshot( filename, fmt = fmt)
         if (inc) assign( ".snap.n", Inc(.snap.n), envir = .GlobalEnv)
         invisible( filename )
    }

