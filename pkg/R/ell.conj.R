ell.conj <-
function( center, shape, dir, radius = 1, len = 1) {
            # returns conjugate axes or tangent lines to ellipse
            vecs <- uv( shape, dir, radius)
            list( u = list( center-len*vecs$u, center+len*vecs$u),
                  v = list( center-len*vecs$v, center+len*vecs$v),
                  tan1 = list( center + vecs$u - len*vecs$v,center + vecs$u+ len*vecs$v),
                  tan2 = list( center - vecs$u - len*vecs$v,center - vecs$u+ len*vecs$v),
                  tan3 = list( center + vecs$v - len*vecs$u,center + vecs$v+ len*vecs$u),
                  tan4 = list( center - vecs$v - len*vecs$u,center - vecs$v+ len*vecs$u),
                  center = center)
    }

