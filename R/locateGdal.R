
library(gdalUtils)
commonLinuxGdalPath <- "/usr/bin"
commonWindowsGdalPath <- "C:/OSGEO4~1/bin/"
if (dir.exists(commonLinuxGdalPath)) {
  gdal_setInstallation(commonLinuxGdalPath)
} else {
  if (dir.exists(commonWindowsGdalPath)) {
    gdal_setInstallation(commonWindowsGdalPath)
    if (is.null(getOption("gdalUtils_gdalPath"))) {
      gdal_setInstallation(rescan = TRUE)
    }
  }
}
if (is.null(getOption("gdalUtils_gdalPath"))) {
  gdal_setInstallation(rescan = TRUE)
}

