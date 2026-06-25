# _local.R -- per-user/host knobs for the targets pipeline.
#
# Sourced FIRST in `_targets.R`, BEFORE tar_source(), so values read at pipeline
# DEFINITION time are set. Crew workers do NOT source this file: anything needed
# at RUN time inside a worker must be passed explicitly into a target (or set in
# .Rprofile). Do not set the same value in two places.

local <- list(
  ## Phase-0 spike: ONE small study area, single replicate.
  ## SprayLake is the smallest Alberta FMA (~2,485 km^2). NB: it has a special-case
  ## branch in LandWeb_preamble.R; Edson (~2,660 km^2) is the next-smallest AB FMA
  ## without special-casing if a cleaner path is wanted.
  study_areas = c("SprayLake"),
  n_reps = 1L,

  ## SpaDES paths. Heavy IO belongs on docker-visible NVMe scratch, not /home or NFS.
  paths = list(
    modulePath = "modules",
    packagePath = "packages",
    inputPath = "inputs",
    outputPath = "outputs",
    scratchPath = file.path(tempdir(), "scratch", "LandWeb")
  ),

  ## within-rep data.table threads (pinned per worker; workers don't source this file)
  dt_threads = 2L,

  ## raster resolution / pixel size (m); LandWeb default 240 (also supports 120)
  ## TODO: evaluate whether running at 120 m is an improvement and practical
  ## (output quality vs ~4x RAM/scratch/runtime). Must be applied GLOBALLY (all
  ## study areas at the same resolution) so results are comparable -- an
  ## all-or-nothing fleet-wide choice, not per-area. See Part F.
  res = 240L,

  ## simulation length (years)
  sim_end = 1000L
)
