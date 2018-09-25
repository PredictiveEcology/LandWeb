if (packageVersion("reproducible") < "0.2.3") {
  devtools::install_github("PredictiveEcology/reproducible@development")
}
if (packageVersion("SpaDES.core") < "0.2.0") {
  devtools::install_github("PredictiveEcology/SpaDES.core@development")
}
if (packageVersion("SpaDES.shiny") < "0.1.0.9013") {
  devtools::install_github("PredictiveEcology/SpaDES.shiny@generalize-modules")
}

DEVMODE <- "DMI"
skipNonDMI <- TRUE

#source("params/DMI_parameters.R") # moved to global.R
file.copy("global_file.R", "global.R", overwrite = TRUE)
shiny::runApp(".", launch.browser = TRUE, port = 5291)
