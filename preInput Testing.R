# Testing SpaDES.core::prepInputs()

require(SpaDES)
require(reproducible)
require(raster)

url.ageMap <- "ftp://ftp.daac.ornl.gov/data/nacp/NA_TreeAge//data/can_age04_1km.tif"
url.vegMap <- "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"
url.studyArea <- paste0("http://www.bcstats.gov.bc.ca/Files/18885d4f-e4cf-443b-bb3b-d169651be62d",
                       "/Boundaries-CensusDivisions2011.zip")
tempPath.ageMap <- file.path(tempdir(), "ageMap")
tempPath.vegMap <- file.path(tempdir(), "vegMap")
tempPath.studyArea <- file.path(tempdir(), "studyArea")
specificAreaToCropShapefile = "Vancouver Island"


# TEMPLATE RASTER
templateRaster <- SpaDES.core::prepInputs(url = url.vegMap,
                            destinationPath = tempPath.vegMap)

raster::plot(templateRaster)  # WORKS

# STUDY AREA
studyArea <- SpaDES.core::prepInputs(url = url.studyArea,
                           destinationPath = tempPath.studyArea,
                           rasterToMatch = templateRaster) %>%
      selectSpecificAreas(specificAreas = specificAreaToCropShapefile)
    unlink(tempPath.studyArea, recursive = TRUE)
    
plot(studyArea) # WORKS

# VEGETATION MAP
# This doesn't crop to extent, but masks (keep extent from original raster) (maybe because I am using rasterToMatch?)
vegMap1 <- SpaDES.core::prepInputs(url = url.vegMap,
                        targetFile = file.path(tempPath.vegMap, "LCC2005_V1_4a.tif"),
                        destinationPath = tempPath.vegMap,
                        rasterToMatch = templateRaster,
                        studyArea = studyArea)
    
# This crops, but doen't masks? doesn't have the legend?
vegMap2 <- SpaDES.core::prepInputs(url = url.vegMap,
                        targetFile = file.path(tempPath.vegMap, "LCC2005_V1_4a.tif"),
                        destinationPath = tempPath.vegMap,
                        studyArea = studyArea)
    
# NOTE: I will use the same raster (vegMap) and just fastMask to studyArea so I don't duplicate the file

# AGE MAP
# Doesn't work:   Don't know which file to load. Please specify targetFile ::  Error in if (x == "" | x == ".") { : argument is of length zero 
ageMap1 <- SpaDES.core::prepInputs(destinationPath = file.path(getwd(), "modules/prepingInputs/data/can_age04_1km.tif"),
                        studyArea = studyArea)

#Error in .local(.Object, ...) : Cannot create a RasterLayer object from this file. (file does not exist) 
ageMap2 <- SpaDES.core::prepInputs(destinationPath = file.path(getwd(), "modules/prepingInputs/data/can_age04_1km.tif"),
                                   targetFile = file.path(getwd(), "modules/prepingInputs/data/can_age04_1km.tif"),
                                   studyArea = studyArea)
file.exists(file.path(getwd(), "modules/prepingInputs/data/can_age04_1km.tif")) # TRUE

# Error: file.exists(checksumFile) is not TRUE 
ageMap3 <- SpaDES.core::prepInputs(targetFile = file.path(getwd(), "modules/prepingInputs/data/can_age04_1km.tif"),
                         studyArea = studyArea)


# ======== FUNCTIONS =========

selectSpecificAreas <- function(studyArea = studyArea,
                                specificAreas = "Vancouver Island"){
  
  
  if (specificAreas == "Vancouver Island"){  
    require(sf)
    require(dplyr)
    require(RColorBrewer)
    
    shp <- studyArea
    positions <- c(3,8,9,15,19,24,28) # Manually selected districts for the analysis
    
    libs <- c("rgdal", "maptools", "gridExtra")
    lapply(libs, require, character.only = TRUE)
    
    new.shp <- shp[shp@data$CDNAME %in% shp@data$CDNAME[positions],]
    id <- new.shp$PRUID
    vc.union <- unionSpatialPolygons(new.shp, id)
    
    return(invisible(vc.union))
    
  } else
    warning("specificAreas needs to be manually coded. It only works automatically for 'Vancouver Island'")
}
