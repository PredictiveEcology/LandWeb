# source("R/locateGdal.R") # Get and locate Gdal installation, likely only run once on a machine
# source("R/updateSpaDES@developmentPkgs.R") # SpaDES.core, SpaDES.shiny, reproducible, quickPlot, webDatabases, SpaDES.tools
# source("generateNewApp.R") # create a new app based on Appsilon's work
# source("params/SK_parameters.R") # endTime, authenticationType, subStudyRegionName, etc.
source("params/Development_Parameters.R") # endTime, authenticationType, subStudyRegionName, etc.
file.copy("global_file.R", "global.R", overwrite = TRUE)
shiny::runApp(".", launch.browser = TRUE, port = 5921)
