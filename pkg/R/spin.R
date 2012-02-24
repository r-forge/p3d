##
## p3d:spin.R
## 2011-12-22
##

spin <-
function( theta=0, phi=15,
          fov = par3d("FOV"), zoom = par3d("zoom"), 
          scale = par3d("scale"), stay = FALSE) {
          
          # Like rgl viewpoint but starts with defaults except
          # for theta (rotation around vertical axis)
          # and phi (angular height above the horisontal plane) 
          # PLAN:
          # without arguments it should just start spinning
          # USAGE: see rgl.viewpoint and rgl.snapshot
          # rgl.bringtotop(stay = FALSE)
          rgl.viewpoint( theta=theta, phi = phi,
                fov = fov, zoom = zoom, scale = scale)

}

