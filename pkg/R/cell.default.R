cell.default <-
function (model, which.coef, levels = 0.95, Scheffe = FALSE, dfn = 2,
        center.pch = 19, center.cex = 1.5, segments = 51, xlab, ylab,
        las = par("las"), col = palette()[2], lwd = 2, lty = 1,
        add = FALSE, ...)
    {

        #require(car)
        which.coef <- if (length(coefficients(model)) == 2)
            c(1, 2)
        else {
            if (missing(which.coef)) {
                if (any(names(coefficients(model)) == "(Intercept)"))
                    c(2, 3)
                else c(1, 2)
            }
            else which.coef
        }
        coef <- coefficients(model)[which.coef]
        xlab <- if (missing(xlab))
            paste(names(coef)[1], "coefficient")
        ylab <- if (missing(ylab))
            paste(names(coef)[2], "coefficient")
        if(missing(dfn)) {
            if (Scheffe) dfn <- sum(df.terms(model))
            else 2
        }
        dfd <- df.residual(model)
        shape <- vcov(model)[which.coef, which.coef]
        ret <- numeric(0)

        ret <- ell( coef, shape, sqrt(dfn * qf(levels, dfn, dfd)))
        colnames(ret) <- c(xlab, ylab)
        ret
    }

