gdalSet <- function() {
  gdal_setInstallation()
  getOption("gdalUtils_gdalPath")
}

source(file.path("R", "simInitAndExperiment.R"))
