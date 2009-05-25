disp <-
function( x ) {
    cat("\n::: ",deparse(substitute(x)), " :::\n")
    print(x)
}

