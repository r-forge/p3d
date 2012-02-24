##
## p3d:Points3d.R
## 2011-12-22
##

# merged Points.*.R (MF)

Points3d <-
function(x,...) UseMethod("Points3d")

Points3d.default <-
		function(x,...) scat3d( x,..., clear =FALSE)

Points3d.formula <-
		function(x,...) Plot3d(x, ... , clear = FALSE)

