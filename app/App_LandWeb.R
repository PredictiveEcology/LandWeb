## requires older version of shiny (see #32)
if (packageVersion("shiny") > "1.0.5") {
  install.packages("https://cran.r-project.org/src/contrib/Archive/shiny/shiny_1.0.5.tar.gz", repos = NULL)
}

if (packageVersion("reproducible") < "0.2.0.9000") {
  install_github("PredictiveEcology/reproducible@development")
}

# source("R/locateGdal.R")
# source("R/updateSpaDES@developmentPkgs.R")
# source("R/developmentParameters.R") # endTime, authenticationType
# source("generateNewApp.R")
source("params/LandWeb_parameters.R")
file.copy("global_file.R", "global.R", overwrite = TRUE)

load_all("~/Documents/GitHub/SpaDES.shiny")
shiny::runApp(".", launch.browser = TRUE, port = 5921)
