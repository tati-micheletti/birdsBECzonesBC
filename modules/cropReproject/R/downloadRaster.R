# downloadRaster function

downloadRaster <- function(sim = sim){
  temp <- tempfile()
  tempDir <- tempdir()
  download.file("http://www.cec.org/sites/default/files/Atlas/Files/Land_Cover_2010/Land_Cover_2010_TIFF.zip",temp)
  unzip(temp, exdir = tempDir)
  from <- file.path(tempDir, "Land_Cover_2010_TIFF", "LandCover_2010", "data", "NA_LandCover_2010_25haMMU.tif")
  to <- file.path(outputPath(sim), "rasterExample.tif")
  file.rename(from = from,  to = to)
return(to)
}
