
selectSpecificAreas <- function(studyArea = sim$studyArea,
                                specificAreas = "Vancouver Island"){


  if (specificAreas == "Vancouver Island"){  
require(bcmaps)
require(sf)
require(dplyr)
require(RColorBrewer)

  if(!any(grepl(x = installed.packages()[,1], pattern = "bcmaps.rdata"))){
    install.packages('bcmaps.rdata', repos='https://bcgov.github.io/drat/')
}

shp <- studyArea
positions <- c(3,8,9,15,19,24,28) # Manually selected districts for the analysis

# TO MAKE THE MAP COLORFUL:
#qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
#col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#names <- shp$CDNAME

# for (i in positions){
#   plot(shp[shp$CDNAME == names[i], ], col = col_vector[i], add = TRUE)
# }

libs <- c("rgdal", "maptools", "gridExtra")
lapply(libs, require, character.only = TRUE)

new.shp <- shp[shp@data$CDNAME %in% shp@data$CDNAME[positions],]
id <- new.shp$PRUID
vc.union <- unionSpatialPolygons(new.shp, id)

return(invisible(vc.union))
  
  } else
    warning("specificAreas needs to be manually coded. It only works automatically for 'Vancouver Island'")
}
