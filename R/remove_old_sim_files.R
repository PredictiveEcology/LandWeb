outputDirs <- c(
  "outputs/BlueRidge_highDispersal_logROS",
  "outputs/Sundre_highDispersal_logROS",
  "outputs/Tolko_AB_N_aspenDispersal_logROS"
)

lapply(outputDirs, function(d) {
  fs::dir_ls(d, regexp = paste0("vegTypeMap|standAgeMap|rstTimeSinceFire"), recurse = 1, type = "file") |>
    grep("year([0-9]){3}[.](grd|gri|tif)", x = _, value = TRUE) |>
    fs::file_info() |>
    dplyr::filter(as.Date(modification_time) < as.Date("2025-01-15")) |>
    dplyr::pull(path) |>
    fs::file_delete()
})

lapply(outputDirs, function(d) {
  fs::dir_ls(d, regexp = paste0(c(
    "Abie_sp[.]tif",
    "Pice_gla[.]tif",
    "Pice_mar[.]tif",
    "Pinu_sp[.]tif",
    "Popu_sp[.]tif",
    "CASFRIAbie_sp[.]tif",
    "CASFRIPice_gla[.]tif",
    "CASFRIPice_mar[.]tif",
    "CASFRIPinu_sp[.]tif",
    "CASFRIPopu_sp[.]tif"
  ) , collapse = "|"), recurse = 1, type = "file") |>
    fs::file_info() |>
    dplyr::filter(as.Date(modification_time) < as.Date("2024-01-01")) |>
    dplyr::pull(path) |>
    fs::file_delete()
})

lapply(outputDirs, function(d) {
  fs::dir_ls(d, regexp = paste0(c(
    "CHECKSUMS[.]txt",
    "mySimOut[.]rds",
    "mySimOut_0[0-9]00[.]rds",
    "rstFlammable_year1000[.]grd",
    "rstFlammable_year1000[.]gri"
  ) , collapse = "|"), recurse = 1, type = "file") |>
    fs::file_info() |>
    dplyr::filter(as.Date(modification_time) < as.Date("2024-01-01")) |>
    dplyr::pull(path) |>
    fs::file_delete()
})
