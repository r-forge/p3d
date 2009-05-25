cell.glh <-
function (obj, which.coef = 1:2, levels = 0.95, Scheffe = FALSE, dfn = 2,
        center.pch = 19, center.cex = 1.5, segments = 51, xlab, ylab,
        las = par("las"), col = palette()[2], lwd = 2, lty = 1,
        add = FALSE, ...)
    {

    # BUGS: works only on first element of glh list
    # glh should be restructured to have two classes: waldList and wald

        obj <- obj[[1]]
        coef <- obj$coef[which.coef]
        xlab <- if (missing(xlab))
            paste(names(coef)[1], "coefficient")
        ylab <- if (missing(ylab))
            paste(names(coef)[2], "coefficient")

        dfd <- obj$anova$denDF
        shape <- obj$vcov[which.coef, which.coef]
        ret <- ell( coef, shape , sqrt( dfn * qf( levels, dfn, dfd)))
        colnames(ret) <- c(xlab, ylab)
        ret
    }

