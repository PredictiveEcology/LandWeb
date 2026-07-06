# _local.R -- per-user/host knobs for the targets pipeline.
#
# Sourced FIRST in `_targets.R`, BEFORE tar_source(), so values read at pipeline
# DEFINITION time are set. Crew workers do NOT source this file: anything needed
# at RUN time inside a worker must be passed explicitly into a target (or set in
# .Rprofile). Do not set the same value in two places.

local <- list(
  ## Phase-0 spike: ONE small study area, 5 stochastic replicates (mainSim branches
  ## over rep_index via pattern = map(rep_index)).
  ## SprayLake is the smallest Alberta FMA (~2,485 km^2). NB: it has a special-case
  ## branch in LandWeb_preamble.R; Edson (~2,660 km^2) is the next-smallest AB FMA
  ## without special-casing if a cleaner path is wanted.
  study_areas = c("SprayLake"),
  n_reps = 5L,

  ## SpaDES paths. Heavy IO belongs on docker-visible NVMe scratch, not /home or NFS.
  ## TODO (after a preamble run completes): these paths are NOT reaching the run --
  ## inputPath(sim) resolved to /mnt/projects/HRV/LandWeb/inputs, not "inputs" below.
  ## Fix the _local.R -> simInitAndSpades(paths=) propagation so inputs/scratch land
  ## where _local.R says (esp. for the docker-visible-NVMe scratch story, Part E).
  paths = list(
    modulePath = "modules",
    inputPath = "inputs",
    outputPath = "outputs",
    ## FIXED (deterministic) compute-node NVMe scratch base, NOT file.path(tempdir(), ...): tempdir()
    ## changes every R session, baking a fresh path into every tar_simspades command and invalidating
    ## the WHOLE pipeline each run. The nodes have dedicated /mnt/scratch; run_simspades isolates +
    ## cleans up a per-run subdir under this base after each phase. (The control node has no
    ## /mnt/scratch but runs no modules, so it never uses this.)
    scratchPath = file.path("/mnt/scratch", Sys.info()[["user"]], "LandWeb")
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

## Gated "extended analyses" -- opt-in, OFF by default. When TRUE, the pipeline
## adds the SCANFI study-area vegetation summary (a cache-aware, pre-seeded
## format="file" target -- it does NOT re-run the multi-hour domain scan) and its
## Quarto report. Workers do not source this file, so the gate (and the quarto
## inspection in tar_quarto) only fires on the control node. Enable for a single
## run via the LANDWEB_EXTENDED_ANALYSES env var, or flip the default below.
options(
  landweb.extended_analyses = isTRUE(as.logical(Sys.getenv("LANDWEB_EXTENDED_ANALYSES", "FALSE")))
)
