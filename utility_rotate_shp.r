library(sf)
library(ggplot2)


# Rotate an sf geom around a center point. If no center is
# specified then it rotates around the center of the geom.
# This is technically an affine transformation: https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations-1
st_ellide_rotate = function(x, degrees, center_coords=NULL){
  if(degrees < -360 | degrees > 360) stop('Degrees must be in the range -360 to 360')
  x = sf::st_combine(x)
  if(is.null(center_coords)){
    center_coords = sf::st_centroid(x)
  }
  radians = degrees * pi/180
  transform_matrix = matrix(c(cos(radians), sin(radians), -sin(radians), cos(radians)), 2, 2)
  
  return((x-center_coords) * transform_matrix + center_coords)
}