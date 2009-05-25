cell <-
function( ... )  {
            UseMethod("cell")
            help <- "
            See help for car::confidence.ellipse.lm
            except that 'cell' returns the points to form the ellipse
            which must be plotted with plot(...,type='l') or lines(...)
            -- Use dfn to determine Sheffe dimension, i.e. dfn = 1 to generate ordinary CIs, dfn = 2 for 2-dim CE, etc.
            -- TODO: extend to 3 dimensions if which.coef has length 3
            "
    }

