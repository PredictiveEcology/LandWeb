testthat::local_edition(3)

## build_upload_bundle() (R/upload_bundle.R) on a small fake outputs/<sa>/ in a temporary directory.

skip_if_not_installed("zip")

root <- test_path("..", "..")
source(file.path(root, "R", "upload_bundle.R"), local = TRUE)

## A fake run of study area "X" below `dir`: what report 05, NRV_summary and burnSummaries write,
## plus what must stay local (aggregates, reporting maps, burn rasters, logs). Returns the values
## of the three upstream targets.
fake_run <- function(dir) {
  put <- function(path, text = path) {
    f <- file.path(dir, path)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    writeLines(text, f)
    path
  }
  report <- c(
    put("outputs/X/reports/05-X.pdf"),
    put("outputs/X/reports/figures/studyArea.png"),
    put("outputs/X/reports/figures/FMA_types.png")
  )
  nrv <- c(
    put("outputs/X/postprocess/csv/lm/Layer One/lm_area.csv"),
    put("outputs/X/postprocess/csv/boxplots/Layer One/leading.csv"),
    put("outputs/X/postprocess/figures/lm/Layer One/area.png"),
    put("outputs/X/postprocess/figures/lm/Layer One/per-metric/core.png"),
    put("outputs/X/postprocess/figures/pm/Layer One/ca.png"),
    put("outputs/X/postprocess/_aggregates/lm_LayerOne/replicate=rep01/part-0.parquet"),
    put("outputs/X/postprocess/_reportingMaps/rep01/vegTypeMap_year0000.tif")
  )
  burn <- c(
    put("outputs/X/mainSim/burnSummaries_fireSizes_allReps.csv"),
    put("outputs/X/mainSim/burnSummaries_fireRegime.csv"),
    put("outputs/X/mainSim/meanAnnualCumulBurnMap.tif")
  )
  put("outputs/X/mainSim/figures/burnSummaries/cumulative_burn_maps.png")
  put("outputs/X/logs/summaries_nrv.log")
  list(report = report, nrv = nrv, burn = burn)
}

bundle <- function(run, ...) {
  build_upload_bundle(
    study_area = "X",
    report = run$report,
    nrv_files = run$nrv,
    burn_files = run$burn,
    n_reps = 50L,
    sim_end = 1000L,
    ...
  )
}

expected <- c(
  "05-X.pdf",
  "RUN_INFO.md",
  "X_figures_lm.zip",
  "X_figures_pm.zip",
  "X_tables.zip",
  "burnSummaries_fireRegime.csv",
  "burnSummaries_fireSizes_allReps.csv",
  "figures/FMA_types.png",
  "figures/cumulative_burn_maps.png",
  "figures/lm/Layer One/area.png",
  "figures/lm/Layer One/per-metric/core.png",
  "figures/pm/Layer One/ca.png",
  "figures/studyArea.png"
)

test_that("the bundle holds exactly the files to upload, laid out as on Drive", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  run <- fake_run(dir)
  files <- bundle(run, project_dir = dir)
  expect_true(all(startsWith(files, "outputs/X/upload/")))
  expect_setequal(substring(files, nchar("outputs/X/upload/") + 1L), expected)
  expect_true(all(file.exists(files)))
  ## nothing else is in the bundle directory
  expect_setequal(list.files("outputs/X/upload", recursive = TRUE), expected)
  ## left out: aggregates, reporting maps, rasters, logs
  expect_false(any(grepl("_aggregates|_reportingMaps|[.]tif$|[.]parquet$|[.]log$", files)))
  ## loose files are the run's own files
  expect_identical(
    unname(tools::md5sum("outputs/X/upload/figures/lm/Layer One/area.png")),
    unname(tools::md5sum("outputs/X/postprocess/figures/lm/Layer One/area.png"))
  )
})

test_that("each zip holds its folder's files, named below that folder", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  run <- fake_run(dir)
  bundle(run, project_dir = dir)
  members <- function(z) sort(zip::zip_list(file.path("outputs/X/upload", z))$filename)
  expect_identical(
    members("X_figures_lm.zip"),
    c("lm/Layer One/area.png", "lm/Layer One/per-metric/core.png")
  )
  expect_identical(members("X_figures_pm.zip"), "pm/Layer One/ca.png")
  expect_identical(
    members("X_tables.zip"),
    c("csv/boxplots/Layer One/leading.csv", "csv/lm/Layer One/lm_area.csv")
  )
})

test_that("a rebuild from the same files gives the same zips and drops stale files", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  run <- fake_run(dir)
  first <- bundle(run, project_dir = dir)
  zips <- grep("[.]zip$", first, value = TRUE)
  md5 <- tools::md5sum(zips)
  writeLines("stale", "outputs/X/upload/figures/old.png")
  second <- bundle(run, project_dir = dir)
  expect_identical(second, first)
  expect_identical(tools::md5sum(zips), md5)
  expect_false(file.exists("outputs/X/upload/figures/old.png"))
})

test_that("RUN_INFO.md says which run the files come from, and names no host or home path", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  run <- fake_run(dir)
  bundle(run, project_dir = dir)
  txt <- readLines("outputs/X/upload/RUN_INFO.md")
  expect_true(any(grepl("50 replicates of 1000 simulated years", txt, fixed = TRUE)))
  expect_true(any(grepl(format(Sys.Date(), "%Y"), txt, fixed = TRUE)))
  all_txt <- paste(txt, collapse = "\n")
  expect_false(grepl(Sys.info()[["nodename"]], all_txt, fixed = TRUE))
  expect_false(grepl(normalizePath("~"), all_txt, fixed = TRUE))
  expect_false(grepl(normalizePath(dir), all_txt, fixed = TRUE))
  expect_false(grepl("sessionInfo|R_LIBS|[.]libPaths", all_txt))
})

test_that("the bundle directory must be named upload, and bundle paths must be unique", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  run <- fake_run(dir)
  expect_error(bundle(run, out_dir = "outputs/X"), "must be named 'upload'")
  expect_true(dir.exists("outputs/X/postprocess"))
  ## a report figure with a burn-summary figure's name
  clash <- file.path(dir, "outputs/X/reports/figures/cumulative_burn_maps.png")
  writeLines("clash", clash)
  run$report <- c(run$report, "outputs/X/reports/figures/cumulative_burn_maps.png")
  expect_error(bundle(run, project_dir = dir), "two files for figures/cumulative_burn_maps.png")
})

test_that("submodule status lines become a table of components", {
  got <- parse_submodule_status(c(
    " abc123 modules/LandMine (v1.0)",
    "+def456 packages/nrvtools (heads/main)",
    "-0789ab packages/crew.ssh",
    ""
  ))
  expect_identical(got$component, c("LandMine", "crew.ssh", "nrvtools"))
  expect_identical(got$kind, c("module", "package", "package"))
  expect_identical(got$commit, c("abc123", "0789ab", "def456"))
  expect_identical(got$state, c("as pinned", "not checked out", "differs from pinned"))
  expect_identical(nrow(parse_submodule_status(character())), 0L)
})

test_that("RUN_INFO.md text marks what could not be read", {
  txt <- format_run_info(list(
    study_area = "X",
    n_reps = 50L,
    sim_end = 1000L,
    outputs_date = NA_character_,
    report_date = "2026-11-24",
    built = "2026-11-25 10:00 UTC",
    commit = NA_character_,
    branch = NA_character_,
    renv_lock_md5 = NA_character_,
    components = parse_submodule_status(" abc modules/LandMine (v1)")
  ))
  expect_true("# LandWeb v3 results: X" %in% txt)
  expect_true("- Summaries of the simulations written: unknown" %in% txt)
  expect_true("- LandWeb commit: `unknown` (branch unknown)" %in% txt)
  expect_true("| LandMine | module | `abc` | as pinned |" %in% txt)
})
