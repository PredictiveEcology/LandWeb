
# source("R/locateGdal.R")
# source("R/updateSpaDES@developmentPkgs.R")
# source("R/developmentParameters.R") # endTime, authenticationType
# source("generateNewApp.R")
source("params/NWT_parameters.R")
file.copy("global_file.R", "global.R", overwrite = TRUE)
shiny::runApp(".", launch.browser = TRUE, port = 5921)
