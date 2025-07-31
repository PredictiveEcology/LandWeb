outputDirs <- file.path("outputs", c(
  "BlueRidge_highDispersal_logROS",
  "Sundre_highDispersal_logROS",
  "Tolko_AB_N_aspenDispersal_logROS"
))

lapply(outputDirs, function(d) {
  LandWebUtils::findOldSimFiles(d, before = "2025-01-15") |>
    fs::file_delete()
})
