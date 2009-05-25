Plot3d.lm <-
function( fit , ...) {
    Plot3d(formula(fit), model.frame(fit), ...)
    Fit3d( fit)
}

