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
      drive_download(file.path("BAM",dataName), path = dPath, overwrite = FALSE, verbose = FALSE)
      if (grepl(x = dPath, pattern = ".RData")){
        birdData <- data.table(load(dPath))
          birdData <-  get(birdData[,V1])
      } else if (grepl(x = dPath, pattern = ".rds")){
        birdData <- data.table(readRDS(dPath))
      } else if (grepl(x = dPath, pattern = ".csv")){
        birdData <- fread(dPath)
      } else stop("The only accepted data formats for now are: '.RData', '.csv', '.rds'")
    }
  
  if (!any(names(birdData)=="X")&!file.exists(lPath)){
    invisible(readline(prompt= paste0("Location (X, Y) was not found in data file. ", 
                                      "Please make sure you have the location dataset ('*.RData', '*.csv', '*.rds')", 
                                      "with at least X, Y and 'SS_derived' or equivalent 'SS' ",
                                      "in Google Drives folder 'BAM', and press [enter] to continue")))
    require(googledrive)
    drive_download(file.path("BAM",data), path = lPath, overwrite = FALSE, verbose = FALSE)
    if (grepl(x = lPath, pattern = ".RData")){
      locationData <- data.table(load(lPath))
      locationData <-  get(locationData[,V1])
    } else if (grepl(x = lPath, pattern = ".rds")){
      locationData <- data.table(readRDS(lPath))
    } else if (grepl(x = lPath, pattern = ".csv")){
      locationData <- fread(lPath)
    } else stop("The only accepted data formats for now are: '.RData', '.csv', '.rds'")

browser()

# Merge X Y to birdData

  }

    xmin <- raster::extent(studyArea)[1]
    xmax <- raster::extent(studyArea)[2]
    ymin <- raster::extent(studyArea)[3]
    ymax <- raster::extent(studyArea)[4]
    
    birdData <- birdData[birdData$X>xmin & birdData$X<xmax &
                   birdData$Y>ymin & birdData$Y<ymax,]
    
    if (nrow(birdData)==0){
      stop("The selected area doesn't contain data. Try increasing the area.")
    }
  
  return(birdData)
}