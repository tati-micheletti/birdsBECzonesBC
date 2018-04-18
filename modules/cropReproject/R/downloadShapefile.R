downloadShapefile <- function(sim = sim){

  temp <- tempfile()
  tempDir <- tempdir()
  download.file("http://www.bcstats.gov.bc.ca/Files/18885d4f-e4cf-443b-bb3b-d169651be62d/Boundaries-CensusDivisions2011.zip",temp)
  unzip(temp, exdir = tempDir)
  from <- file.path(paths$modulePath, "cropReproject", "data", "CD_2011.shp")
  to <- file.path(inputPath(sim), "studyArea.tif")
  file.rename(from = from,  to = to)
  return(to)

}
