if (!require("profvis", character.only = TRUE, quietly = TRUE)) {
  install.packages("profvis")
}

#load_all("~/GitHub/PredictiveEcology/SpaDES.shiny")

DEVMODE <- TRUE
PROFVIS <- FALSE
runName <- "tolko_SK_doubleFRI"
useCache342 <- FALSE ## set this to FALSE to run model from scratch

file.copy("global_file.R", "global.R", overwrite = TRUE)
if (exists("PROFVIS") && isTRUE(PROFVIS)) {
  profvis::profvis({
    shiny::runApp()
  })
} else {
  shiny::runApp(".", launch.browser = TRUE, port = 5921)
}
