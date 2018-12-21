#git clone git@github.com:eliotmcintire/LandWeb --branch rewriteNoAll --recurse-submodules -j8

#devtools::install_github("PredictiveEcology/quickPlot", ref = "master", dependencies = FALSE)
devtools::install_github("PredictiveEcology/reproducible", ref = "development", dependencies = FALSE)
#devtools::install_github("PredictiveEcology/webDatabases", ref = "master", dependencies = FALSE)
devtools::install_github("PredictiveEcology/SpaDES.tools", ref = "development", dependencies = FALSE)
devtools::install_github("PredictiveEcology/SpaDES.core", ref = "development", dependencies = FALSE)
devtools::install_github("PredictiveEcology/pemisc", ref = "development", dependencies = FALSE)
devtools::install_github("PredictiveEcology/map", ref = "master", dependencies = FALSE)
devtools::install_github("PredictiveEcology/LandR", ref = "development", dependencies = FALSE)
devtools::install_github("PredictiveEcology/SpaDES.shiny", ref = "development", dependencies = FALSE)

if (FALSE) {
  options(shiny.reactlog = TRUE)
}

if (FALSE) {
  try(detach("package:map", unload = TRUE))
  try(detach("package:LandR", unload = TRUE))
  try(detach("package:pemisc", unload = TRUE))
  try(detach("package:SpaDES.core", unload = TRUE))
  try(detach("package:SpaDES.tools", unload = TRUE))
  try(detach("package:reproducible", unload = TRUE))

  ghPath <- "~/GitHub"

  if (Sys.info()[["user"]] == "achubaty")
    ghPath <- "~/GitHub/PredictiveEcology"

  devtools::load_all(file.path(ghPath, "reproducible"))
  devtools::load_all(file.path(ghPath, "pemisc"))
  devtools::load_all(file.path(ghPath, "map"))
  devtools::load_all(file.path(ghPath, "SpaDES.core"))
  devtools::load_all(file.path(ghPath, "SpaDES.tools"))
  devtools::load_all(file.path(ghPath, "LandR"))
}

## test download of private data from Google Drive
if (FALSE) {
  dataDir <- file.path("~/GitHub/LandWeb/m/Boreal_LBMRDataPrep/data")

  file.remove(c(
    list.files(dataDir, pattern = "SPP_1990_FILLED_100m_NAD83_LCC_BYTE_VEG", full.names = TRUE),
    list.files(dataDir, pattern = "CASFRI", full.names = TRUE)
  ))
}
