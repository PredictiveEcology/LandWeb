## The run's state for scripts/archive-run.sh: the targets metadata and the shared growth traits once,
## and for each study area the values of its species list, traits, data preparation, replicates and
## summaries (restoring a store snapshot replaces them in the store). Prints "state saved". Run from the
## project root:
##   Rscript-4.6.1 scripts/archive-state.R <archive dir> <study area> [...]
args <- commandArgs(TRUE)
dst <- args[1]
sas <- args[-1]
dir.create(file.path(dst, "_state"), showWarnings = FALSE)
data.table::fwrite(targets::tar_meta(), file.path(dst, "_state", "tar_meta.csv"))
data.table::fwrite(
  targets::tar_read(growthTraits),
  file.path(dst, "_state", "growthTraits.csv")
)
stages <- c(
  "sppList_",
  "speciesTraits_",
  "dataPrep_",
  "mainSim_",
  "summaries_nrv_",
  "summaries_burn_"
)
for (sa in sas) {
  d <- file.path(dst, sa, "state")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  for (nm in paste0(stages, sa)) {
    qs2::qs_save(targets::tar_read_raw(nm), file.path(d, paste0(nm, ".qs2")))
  }
}
cat("state saved\n")
