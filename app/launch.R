load_all("~/GitHub/PredictiveEcology/SpaDES.shiny/")

appPath <- "~/GitHub/LandWeb/app"
setwd(appPath)
file.copy("global_file.R", "global.R", overwrite = TRUE)
shiny::runApp(appPath, launch.browser = TRUE, port = 5129)
