outputDirs <- c(
  "outputs/BlueRidge_highDispersal_logROS",
  "outputs/Sundre_highDispersal_logROS",
  "outputs/Tolko_AB_N_aspenDispersal_logROS"
)

find_old_files <- function(path, regexp, before = "2024-01-01") {
  fs::dir_ls(path, regexp = regexp, recurse = 1, type = "file") |>
    grep("year([0-9]){3}[.](grd|gri|tif)", x = _, value = TRUE) |>
    fs::file_info() |>
    dplyr::filter(as.Date(modification_time) < as.Date(!!before)) |>
    dplyr::pull(path)
}

lapply(outputDirs, function(d) {
  find_old_files(d, paste0("vegTypeMap|standAgeMap|rstTimeSinceFire"), "2024-01-01") |>
    fs::file_delete()
})

lapply(outputDirs, function(d) {
  find_old_files(d, paste0(c(
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
  ) , collapse = "|"), "2024-01-01") |>
    fs::file_delete()
})

lapply(outputDirs, function(d) {
  find_old_files(d, paste0(c(
    "CHECKSUMS[.]txt",
    "mySimOut[.]rds",
    "rstFlammable_year1000[.]grd",
    "rstFlammable_year1000[.]gri"
  ), collapse = "|"), "2024-01-01") |>
    fs::file_delete()
})
