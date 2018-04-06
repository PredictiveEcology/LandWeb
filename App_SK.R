
# source("locateGdal.R")
# source("updateSpaDES@developmentPkgs.R")
# source("developmentParameters.R") # endTime, authenticationType
# source("generateNewApp.R")
source("SK_parameters.R")
file.copy("global_file.R", "global.R", overwrite = TRUE)
shiny::runApp(".", launch.browser = TRUE, port = 5921)
