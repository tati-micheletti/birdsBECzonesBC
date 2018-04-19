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

# reproject studyArea to match latlong
studyAreaToCrop <- sp::spTransform(studyArea, CRSobj = "+init=epsg:4326")

    xmin <- raster::extent(studyAreaToCrop)[1]
    xmax <- raster::extent(studyAreaToCrop)[2]
    ymin <- raster::extent(studyAreaToCrop)[3]
    ymax <- raster::extent(studyAreaToCrop)[4]
    
    browser()
    
  birdData2 <- birdData[birdData$X>xmin & birdData$X<xmax &
                   birdData$Y>ymin & birdData$Y<ymax,] # THERE ARE POINTS (Nicole's map showed it!), I JUST DONT KNOW WHY THESE ARE NOT BEING SELECTED...
    
    if (nrow(birdData)==0){
      stop("The selected area doesn't contain data. Try increasing the area.")
    }
  
  return(birdData)
}