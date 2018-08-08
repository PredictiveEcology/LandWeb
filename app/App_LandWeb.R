if (packageVersion("reproducible") < "0.2.1") {
  install_github("PredictiveEcology/reproducible@development")
}
if (packageVersion("SpaDES.core") < "0.2.0") {
  install_github("PredictiveEcology/SpaDES.core@development")
}
if (packageVersion("SpaDES.shiny") < "0.1.0.9009") {
  install_github("PredictiveEcology/SpaDES.shiny@development")
}

#source("params/LandWeb_parameters.R") # moved to global.R
file.copy("global_file.R", "global.R", overwrite = TRUE)
shiny::runApp(".", launch.browser = TRUE, port = 5921)
