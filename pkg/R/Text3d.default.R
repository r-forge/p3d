Text3d.default <-
function(...,col='blue'){
# disp( list(...,col=col))
 do.call("rgl.texts", args3d(...,col=col))
}
