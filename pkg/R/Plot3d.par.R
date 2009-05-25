Plot3d.par <-
function(..., default = FALSE ) {
    if( default ) assign(".Plot3d.par",
        list(),    # default
        pos = 1)
   # if ( exists(".tt") ) pars <- .tt
   if ( exists(".Plot3d.par") ) pars <- .Plot3d.par
    else pars <- list()
     #    disp(pars)
    if (length( a <- list(...)) == 0) return(pars)
    if (is.null(names(a))) return( pars[unlist(a)] )
    old <- pars[names(a)]
    pars[names(a)] <- a
    assign(".Plot3d.par", pars, pos = 1)
    old
}

