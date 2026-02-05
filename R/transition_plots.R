# create vegetation transition plots ----------------------------------------------------------

library(sf)
library(terra)

library(LandR)
library(SpaDES.core)

library(data.table)
library(dplyr)
library(ggplot2)

## TODO: customize this per study area ----------
# studyAreaRun <- "NW_AB_highDispersal_logROS"
studyAreaRun <- "DMI_aspenDispersal_logROS"
# studyAreaRun <- "Tolko_AB_N_aspenDispersal_logROS"
paths_sim <- list(
  outputPath = file.path("outputs", studyAreaRun, "rep01")
)

## TODO: can't load simlists -- these old sims didn't save them :/
## and the old mySimOut.rds files can't be opened :(
## try getting everything from ml objects

ml <- readRDS(file.path(dirname(paths_sim[["outputPath"]]), "ml_preamble.rds"))

studyAreaReporting <- sf::st_as_sf(ml$`DMI Full`)
# studyAreaReporting <- sf::st_as_sf(ml$`Tolko AB North`)

rasterToMatch <- ml$fireReturnInterval |>
  terra::rast() |>
  terra::crop(studyAreaReporting)

FMUs <- sf::st_as_sf(ml$`AB FMU Boundaries`) |>
  sf::st_crop(studyAreaReporting) |>
  dplyr::mutate(FMU_NAME = as.factor(FMU_NAME))

rstFMUs <- terra::rasterize(FMUs, rasterToMatch, field = "FMU_NAME") |>
  terra::crop(studyAreaReporting, mask = TRUE)
terra::plot(rstFMUs)

rm(ml)

CCVTM_grd <- file.path(dirname(paths_sim[["outputPath"]]), "CurrentConditionVTM.grd")
CCVTM_tif <- file.path(dirname(paths_sim[["outputPath"]]), "CurrentConditionVTM.tif")

if (file.exists(CCVTM_tif)) {
  file.copy(
    CCVTM_tif,
    file.path(paths_sim[["outputPath"]], "vegTypeMap_year0000.tif")
  )
} else if (file.exists(CCVTM_grd)) {
  file.copy(
    CCWTM_grd,
    file.path(paths_sim[["outputPath"]], "vegTypeMap_year0000.grd")
  )

  file.copy(
    file.path(dirname(paths_sim[["outputPath"]]), "CurrentConditionVTM.gri"),
    file.path(paths_sim[["outputPath"]], "vegTypeMap_year0000.gri")
  )
}

## ----------------------------------------------

years <- c(0, seq(700, 1000, 100))

if (file.exists(CCVTM_tif)) {
  fvtm <- c(
    CCVTM_tif,
    file.path(paths_sim[["outputPath"]], sprintf("vegTypeMap_year%04d.grd", years[-1]))
  )
} else if (file.exists(CCVTM_grd)) {
  fvtm <- file.path(paths_sim[["outputPath"]], sprintf("vegTypeMap_year%04d.grd", years))
}

source("R/plot-transitions.R")

transitions_df <- vegTransitions(
  vtm = fvtm,
  ecoregion = rstFMUs,
  field = "FMU_NAME",
  studyArea = studyAreaReporting,
  times = years,
  na.rm = TRUE
)

transition_ggs <- plotVegTransitions(transitions_df)

purrr::walk(
  .x = names(transition_ggs),
  .f = function(i) {
    fgg <- file.path(paths_sim[["outputPath"]], "figures") |> fs::dir_create() |>
      file.path(paste0("transition_vegTypeMap_", i, ".png"))
    ggsave(fgg, transition_ggs[[i]], width = 12, height = 6)
  }
)
