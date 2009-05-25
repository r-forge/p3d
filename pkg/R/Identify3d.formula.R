Identify3d.formula <-
function( formula = attr(data, "formula"),
            data = sys.parent()  , labels = NULL , adj = 0,  ...) {
    env <- environment(formula)
    labels <- eval(substitute(labels), data, env)
    if ( is.null(labels)) labels <- rownames(data)
    dd <- model.frame( formula, data )
    if ( ncol(dd) < 3) stop( "Need at least three variables for identify3d")
    Identify3d( dd[[2]], dd[[1]], dd[[3]], labels = labels, adj = adj, ...)
}

