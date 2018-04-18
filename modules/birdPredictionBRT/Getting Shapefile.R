
require(bcmaps)
require(sf)
require(dplyr)
require(RColorBrewer)

install.packages('bcmaps.rdata', repos='https://bcgov.github.io/drat/')

shp <- raster::shapefile(x = file.path(getwd(),"CD_2011.shp")
                         
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

names <- shp$CDNAME

positions <- c(3,8,9,15,19,24,28) # Manually selected districts for the analysis
plot(st_geometry(bc))
for (i in positions){
  plot(shp[shp$CDNAME == names[i], ], col = col_vector[i], add = TRUE)
}

libs <- c("rgdal", "maptools", "gridExtra")
lapply(libs, require, character.only = TRUE)

id <- new.shp$PRUID
vc.union <- unionSpatialPolygons(new.shp, id)

plot(vc.union)
