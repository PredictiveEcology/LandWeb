if (packageVersion("reproducible") < "0.2.3") {
  install_github("PredictiveEcology/reproducible@development")
}
if (packageVersion("SpaDES.core") < "0.2.0") {
  install_github("PredictiveEcology/SpaDES.core@development")
}
if (packageVersion("SpaDES.shiny") < "0.1.0.9011") {
  install_github("PredictiveEcology/SpaDES.shiny@generalize-modules")
}

#source("params/LandWeb_parameters.R") # moved to global.R
file.copy("global_file.R", "global.R", overwrite = TRUE)
shiny::runApp(".", launch.browser = TRUE, port = 5921)
