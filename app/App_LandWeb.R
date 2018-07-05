if (packageVersion("reproducible") < "0.2.1") {
  install_github("PredictiveEcology/reproducible@development")
}
if (packageVersion("SpaDES.core") < "0.2.0") {
  install_github("PredictiveEcology/SpaDES.core@development")
}

# source("R/locateGdal.R")
# source("R/updateSpaDES@developmentPkgs.R")
# source("R/developmentParameters.R") # endTime, authenticationType
# source("generateNewApp.R")
source("params/LandWeb_parameters.R")
file.copy("global_file.R", "global.R", overwrite = TRUE)
shiny::runApp(".", launch.browser = TRUE, port = 5921)
