if (!require("profvis", character.only = TRUE, quietly = TRUE)) {
  install.packages("profvis")
}

#load_all("~/GitHub/PredictiveEcology/SpaDES.shiny")

DEVMODE <- TRUE
PROFVIS <- FALSE

#source("params/Development_Parameters.R") # moved to global.R
file.copy("global_file.R", "global.R", overwrite = TRUE)
if (exists("PROFVIS") && isTRUE(PROFVIS)) {
  profvis::profvis({
    shiny::runApp()
  })
} else {
  shiny::runApp(".", launch.browser = TRUE, port = 5921)
}
