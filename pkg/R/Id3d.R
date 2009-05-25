Id3d <-
function (labels=row.names(data), pad , ...) {
    pars <- .Plot3d.par
    data <- pars$data
    nams <- pars$names
    if( !missing( pad )) {
        padstring <- do.call(paste, c(as.list(rep(" ", pad)),sep =""))
        labels <- paste( padstring, labels, sep = "")
    }
    if ( pars$has.groups ) {
        Identify3d(
            data[[nams['x']]],  data[[nams['y']]],data[[nams['z']]],
            groups = data[[nams['g']]],labels=labels,...)
    } else {
        Identify3d(
            data[[nams['x']]],  data[[nams['y']]],data[[nams['z']]],
            labels=labels,...)
    }
}

