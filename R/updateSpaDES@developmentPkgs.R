#git clone git@github.com:eliotmcintire/LandWeb --branch rewriteNoAll --recurse-submodules -j8

devtools::install_github("PredictiveEcology/quickPlot", ref = "development", dependencies = FALSE)
devtools::install_github("PredictiveEcology/reproducible", ref = "forMapObject", dependencies = FALSE)
#devtools::install_github("PredictiveEcology/webDatabases", ref = "master", dependencies = FALSE)
devtools::install_github("PredictiveEcology/SpaDES.tools", ref = "development", dependencies = TRUE)
devtools::install_github("PredictiveEcology/SpaDES.core", ref = "newSetClass", dependencies = FALSE)
devtools::install_github("PredictiveEcology/map", ref = "master", dependencies = FALSE)
devtools::install_github("PredictiveEcology/pemisc", ref = "master", dependencies = FALSE)
devtools::install_github("PredictiveEcology/SpaDES.shiny", ref = "development", dependencies = FALSE)

if (FALSE) {
  options(shiny.reactlog = TRUE)
}

## test download of private data from Google Drive
if (FALSE) {
  dataDir <- file.path("~/GitHub/LandWeb/m/Boreal_LBMRDataPrep/data")

  file.remove(c(
    list.files(dataDir, pattern = "SPP_1990_FILLED_100m_NAD83_LCC_BYTE_VEG", full.names = TRUE),
    list.files(dataDir, pattern = "CASFRI", full.names = TRUE)
  ))
}
