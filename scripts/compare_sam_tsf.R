## Compare the three "how old is this stand?" layers a LandWeb run produces, for one replicate:
##
##   SAM (biomass-weighted) -- the saved `standAgeMap_year<YYYY>.tif`; LandR's default weighting,
##                             so a young, high-biomass cohort pulls the pixel's age down.
##   SAM (oldest cohort)    -- recomputed from `cohortData` + `pixelGroupMap` with `weight = NA`,
##                             i.e. the age of the oldest cohort in the pixel.
##   TSF                    -- `rstTimeSinceFire_year<YYYY>.tif`; years since the pixel last burned.
##
## They disagree in ways that matter for the NRV age-class summaries: TSF resets on fire regardless
## of what regenerates, while both SAMs track cohorts, and the two weightings diverge wherever a
## pixel carries cohorts of mixed age. This script plots the three age distributions together and
## faceted, so the disagreement is visible before it propagates into an age-class figure.
##
## Writes to `outputs/<studyArea>/mainSim/figures/` (study-area-specific -- see CLAUDE.md).
##
## RUN WITH `Rscript-4.6.1 scripts/compare_sam_tsf.R [studyArea] [rep] [year]` -- NOT `--vanilla`,
## which skips .Rprofile and so never activates renv. Reads full-extent rasters plus a cohortData
## table and recomputes a stand age map, so run it on a COMPUTE NODE, not the controller.
##
## Defaults match the current Phase-0 run; override positionally, e.g.
##   Rscript-4.6.1 scripts/compare_sam_tsf.R WesternAlbertaUpland 3 1000

suppressMessages({
  library(data.table)
  library(ggplot2)
  library(terra)
})

args <- commandArgs(trailingOnly = TRUE)
studyArea <- if (length(args) >= 1L) args[[1]] else "WesternAlbertaUpland"
repNum <- if (length(args) >= 2L) as.integer(args[[2]]) else 1L
year <- if (length(args) >= 3L) as.integer(args[[3]]) else 1000L

## v3 layout: outputs/<studyArea>/mainSim/rep<NN>/<object>_year<YYYY>.<ext>, year zero-padded to 4.
repDir <- file.path("outputs", studyArea, "mainSim", sprintf("rep%02d", repNum))
outDir <- reproducible::checkPath(file.path("outputs", studyArea, "mainSim", "figures"), create = TRUE)
.f <- function(object, ext) file.path(repDir, sprintf("%s_year%04d.%s", object, year, ext))

stopifnot(dir.exists(repDir))

sam <- terra::rast(.f("standAgeMap", "tif"))
tsf <- terra::rast(.f("rstTimeSinceFire", "tif"))

## v2 saved pixelGroupMap as INT2U, which overflowed the group IDs, so the v2 version of this
## script had to pull the layer out of a saved simList instead. v3 writes it as UInt32 -- the
## .tif is now authoritative and can be read directly.
cd <- qs2::qs_read(.f("cohortData", "qs2"))
pgm <- terra::rast(.f("pixelGroupMap", "tif"))

samOldest <- LandR::standAgeMapGenerator(cd, pgm, weight = NA) |> terra::rast()

.vals <- function(x) terra::values(x, mat = FALSE)
metricLevels <- c("SAM (biomass-weighted)", "SAM (oldest cohort)", "TSF")
df <- data.table(
  age = c(.vals(sam), .vals(samOldest), .vals(tsf)),
  metric = factor(
    rep(metricLevels, times = c(terra::ncell(sam), terra::ncell(samOldest), terra::ncell(tsf))),
    levels = metricLevels
  )
)
df <- df[!is.na(age)]

.base <- function(p) {
  p +
    geom_histogram(alpha = 0.3, position = "identity", bins = 50) +
    labs(
      x = "stand age (years)", y = "pixel count",
      colour = NULL, fill = NULL,
      subtitle = sprintf("%s, rep %02d, year %d", studyArea, repNum, year)
    ) +
    theme_bw()
}

pOverlay <- .base(ggplot(df, aes(x = age, colour = metric, fill = metric)))
pFacet <- .base(ggplot(df, aes(x = age, colour = metric, fill = metric))) +
  facet_wrap(~metric, ncol = 3) +
  theme(legend.position = "none")

.png <- function(plot, tag) {
  fout <- file.path(outDir, sprintf("sam_vs_tsf_%s_rep%02d_year%04d.png", tag, repNum, year))
  ggplot2::ggsave(fout, plot, width = if (tag == "facet") 10 else 7, height = 4, dpi = 150)
  message("wrote ", fout)
}
.png(pOverlay, "overlay")
.png(pFacet, "facet")

## TODO: extend to the current-condition (CC) layers once NRV_summary's reportingPolygons carries
## them (see the module-contract TODO in _targets.R, the "CC SAM"/"CC TSF" entries). Three more
## comparisons fall out: CC-SAM vs CC-TSF; CC-TSF vs simulated TSF; CC-SAM vs simulated SAM --
## the last two being the v3 "does the simulation start where the landscape actually is?" check.
