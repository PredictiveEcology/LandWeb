## NOTE: several packages used here indirectly;
## library() calls included here to appease renv.
##
## renv finds dependencies by static analysis of library()/require()/pkg::fn() in the project's
## R/Rmd/qmd files, so packages reached only through a SpaDES module's `reqdPkgs` -- which renv
## does not parse -- read as "installed y, recorded y, used n" and leave the project permanently
## out-of-sync. That banner is not harmless: it printed on every R invocation and masked a REAL
## problem (a compute node running an out-of-date SpaDES.targets, plus a workflowtools ref
## mismatch) until `renv::status()` was read carefully. Listing them here makes the out-of-sync
## signal mean something again.
##
## NEVER run `renv::snapshot()` in this project -- it has previously cut the lockfile from 437
## entries to 69. Add or update a package with, e.g.:
##   renv::install("FOR-CAST/fireregimetools", lock = TRUE)
## which records the package AND any additional dependencies it pulls in.

if (FALSE) {
  library(fireregimetools) ## used by `burnSummaries` via reqdPkgs
  library(ggforce) ## used by `NRV_summary` via reqdPkgs
  library(nrvtools) ## used by `NRV_summary` via reqdPkgs
  library(quarto) ## used to render reports/*.qmd
  library(tweenr) ## used by `ggforce`
}
