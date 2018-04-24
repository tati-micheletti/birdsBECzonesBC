# loadData Function

loadCroppedData <- function(sim = sim, studyArea = sim$studyArea, 
                            dataPath = file.path(modulePath(sim), "prepingInputs/data"),
                            locationDataName = sim$locationDataName,
                            dataName = sim$dataName){
  require(data.table)
  require(raster)
  require(sf)
  require(reproducible)
  
  dPath <- file.path(dataPath, dataName)
  lPath <- file.path(dataPath, locationDataName)
  
    if (!file.exists(dPath)){
        invisible(readline(prompt="Make sure you have the dataset in Google Drives folder 'BAM', and press [enter] to continue"))
      require(googledrive)
      drive_download(file.path("BAM",dataName), path = dPath, overwrite = FALSE, verbose = FALSE)}
      if (grepl(x = dPath, pattern = ".RData")){
        birdData <- data.table(load(dPath))
          birdData <-  as.data.table(get(birdData[,V1]))
      } else if (grepl(x = dPath, pattern = ".rds")){
        birdData <- as.data.table(readRDS(dPath))
      } else if (grepl(x = dPath, pattern = ".csv")){
        birdData <- fread(dPath)
      } else stop("The only accepted data formats for now are: '.RData', '.csv', '.rds'")

  
  if (!any(names(birdData)=="X")&!file.exists(lPath)){
    invisible(readline(prompt= paste0("Location (X, Y) was not found in data file. ", 
                                      "Please make sure you have the location dataset ('*.RData', '*.csv', '*.rds')", 
                                      "with at least X, Y and 'SS_derived' or equivalent 'SS' ",
                                      "in Google Drives folder 'BAM', and press [enter] to continue")))
    require(googledrive)
    drive_download(file.path("BAM",data), path = lPath, overwrite = FALSE, verbose = FALSE)}
    if (grepl(x = lPath, pattern = ".RData")){
      locationData <- data.table(load(lPath))
      locationData <-  as.data.table(get(locationData[,V1]))
    } else if (grepl(x = lPath, pattern = ".rds")){
      locationData <- as.data.table(readRDS(lPath))
    } else if (grepl(x = lPath, pattern = ".csv")){
      locationData <- fread(lPath)
    } else stop("The only accepted data formats for now are: '.RData', '.csv', '.rds'")

bdSS <- unique(birdData[,SS_derived])
location <- subset(x = locationData, 
                       subset = SS %in% bdSS,
                        select = c(SS,X_coor,Y_coor)) %>%
                    unique()
names(location) <- c("SS_derived", "X", "Y")

birdData <- merge(x = birdData, y = location, by = "SS_derived")

# reproject studyArea to match data

# ============= ALL BELOW FAILED SO FAR ======================

browser()


# Getting only the points

points <- data.frame(X = birdData$X, Y = birdData$Y) %>%
  SpatialPoints()

epsg32610 <- "+init=epsg:32610"  # NEED TO TRY THIS. THIS IS UTM. It is possible this is the projection, considering the data comes from GP
epsg3857 <- "+init=epsg:3857"  # Google maps, etc...

epsg4267 <- "+init=epsg:4267"
epsg4326 <- "+init=epsg:4326"
epsg4269 <- "+init=epsg:4269"
LCC05 <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
LambertsConformalConic <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

# Already tried: original data, all the following projections, transforming both points and rasters and shapefile. 
# Nothing worked.
studyAreaToCrop <- Cache(prepInputs, url = sim$url.studyArea,
                         destinationPath = sim$tempPath.studyArea) %>%
  selectSpecificAreas(specificAreas = sim$specificAreaToCropShapefile)

# TRANSFORMING POINTS (wich appear to not have a projection) --> Still not aligning
pointsDF <- data.frame(X = birdData$X, Y = birdData$Y)
coordinates(pointsDF) <- ~X+Y
projection(pointsDF) <- "+init:epsg=4326"
pointsTrans <- spTransform(pointsDF, CRS(projection(sim$vegMap)))

# TRYING SHAPEFILE SENT BY DIANA --> It's not the original projection from the points 
require(rgdal)
newSHPPath <- "/home/tmichele/Documents/GitHub/birdsBECzonesBC/modules/prepingInputs/data/province_state_lcc.shp"
newSHP <- readOGR(newSHPPath)
naStates <- subset(newSHP, is.na(STATE))

studyAreaToCrop <- sp::spTransform(studyAreaToCrop, CRSobj = LambertsConformalConic)
studyAreaToCropSHP <- sp::spTransform(naStates, CRSobj = epsg4267) #and also tested all other projections...

plot(studyAreaToCrop) #or nStates
plot(points, add= TRUE , col = 'red', pch = 19, cex = 0.5) #or pointsTrans with vegMap plot

# ============= ALL FAILED SO FAR ======================

    xmin <- raster::extent(studyAreaToCrop)[1]
    xmax <- raster::extent(studyAreaToCrop)[2]
    ymin <- raster::extent(studyAreaToCrop)[3]
    ymax <- raster::extent(studyAreaToCrop)[4]
  
  birdData2 <- birdData[birdData$X>xmin & birdData$X<xmax &
                   birdData$Y>ymin & birdData$Y<ymax,] # THERE ARE POINTS (Nicole's map showed it!), I JUST DONT KNOW WHY THESE ARE NOT BEING SELECTED...
    
    if (nrow(birdData)==0){
      stop("The selected area doesn't contain data. Try increasing the area.")
    }
  
  return(birdData)
}