if (packageVersion("reproducible") < "0.2.0.9002") {
  install_github("PredictiveEcology/reproducible@development")
}

# source("R/locateGdal.R")
# source("R/updateSpaDES@developmentPkgs.R")
# source("R/developmentParameters.R") # endTime, authenticationType
# source("generateNewApp.R")
source("params/LandWeb_parameters.R")
file.copy("global_file.R", "global.R", overwrite = TRUE)
shiny::runApp(".", launch.browser = TRUE, port = 5921)
