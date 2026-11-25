testthat::local_edition(3)

## The v2 -> v3 crosswalk and the v2 run lookup (R/fma_boundary_change.R), used by reports 04 and 05.
## The crosswalk's rows were checked against the polygons' overlap; these tests hold its structure and
## the matches that names alone would get wrong.

root <- test_path("..", "..")
fbc <- new.env()
sys.source(file.path(root, "R", "fma_boundary_change.R"), envir = fbc)

test_that("the crosswalk has one primary v2 polygon per tenure, and no gaps", {
  cw <- fbc$v2_v3_crosswalk()
  expect_named(cw, c("tenure", "run", "layer", "zone", "primary", "patch_layer", "basis"))
  expect_false(anyNA(cw))
  expect_true(all(nzchar(as.matrix(cw[c("tenure", "run", "layer", "zone", "patch_layer")]))))
  expect_type(cw$primary, "logical")
  primaries <- cw$tenure[cw$primary]
  expect_false(anyDuplicated(primaries) > 0L)
  expect_setequal(unique(cw$tenure), primaries)
  ## a v2 polygon is one tenure, never two
  expect_false(anyDuplicated(cw[c("run", "layer", "zone")]) > 0L)
})

test_that("each crosswalk row says why it is or is not the run compared", {
  cw <- fbc$v2_v3_crosswalk()
  basis <- fbc$v2_primary_basis()
  expect_true(all(cw$basis %in% names(basis)))
  expect_true(all(cw$basis[!cw$primary] == "alternative"))
  expect_false(any(cw$basis[cw$primary] == "alternative"))
  ## a tenure's only run needs no choice; a choice needs an alternative
  n_runs <- table(cw$tenure[!duplicated(cw[c("tenure", "run")])])
  only <- cw$tenure[cw$primary & cw$basis == "only_run"]
  expect_true(all(n_runs[only] == 1L))
  chosen <- cw$tenure[cw$primary & cw$basis != "only_run"]
  expect_true(all(n_runs[chosen] > 1L))
})

test_that("WF_Edson is compared from the run its 2021 report used", {
  cw <- fbc$v2_v3_crosswalk()
  e <- cw[cw$tenure == "WF_Edson" & cw$primary, ]
  expect_identical(e$run, "WestFraser_S")
  expect_identical(e$basis, "report_2021")
})

test_that("crosswalk codes have the shape of curated NAME_SHORT codes", {
  cw <- fbc$v2_v3_crosswalk()
  expect_true(all(grepl("^[A-Za-z0-9]+(_[A-Za-z0-9]+)*$", cw$tenure)))
  expect_true(all(nchar(cw$tenure) <= 16L))
})

test_that("crosswalk codes are curated LandWebUtils tenure codes", {
  skip_if_not_installed("LandWebUtils")
  expect_true(all(fbc$v2_v3_crosswalk()$tenure %in% LandWebUtils::tenureShortNames()$NAME_SHORT))
})

test_that("every v2 baseline run is matched or explained, and nothing else is", {
  cw <- fbc$v2_v3_crosswalk()
  um <- fbc$v2_unmatched()
  expect_named(um, c("run", "layer", "zone", "reason"))
  expect_setequal(union(cw$run, um$run), fbc$.v2_baseline_runs)
  ## an unmatched polygon is not also matched in the same layer
  key <- function(d) paste(d$run, d$layer, d$zone, sep = "|")
  expect_length(intersect(key(cw), key(um)), 0L)
})

test_that("the crosswalk follows geometry where v2 and v3 names disagree", {
  cw <- fbc$v2_v3_crosswalk()
  zone_of <- function(t) cw$zone[cw$tenure == t & cw$primary]
  expect_identical(zone_of("CanforWhitecourt"), "Millar Western Forest Products Ltd.")
  expect_identical(zone_of("MillarWestern"), "West Fraser Mills Ltd. (Slave Lake)")
  expect_identical(zone_of("SK_MeadowLake"), "Mistik")
  expect_identical(zone_of("SK_BronsonGreen"), "Meadow Lake OSB")
})

test_that("v2_run_dir() takes only the exact baseline run, high dispersal first", {
  v2 <- withr::local_tempdir()
  dirs <- c(
    "ANC_aspenDispersal_logROS",
    "ANC_aspenDispersal_logROS_fri1_res50",
    "ANC_logROS_nosuccession",
    "SprayLake_v3",
    "SprayLake_highDispersal_burnyROS",
    "SprayLake_highDispersal_logROS_lthfc_8c",
    "SprayLake_highDispersal_logROS",
    "FMANWT2_highDispersal_logROS",
    "Both_aspenDispersal_logROS",
    "Both_highDispersal_logROS",
    "Trial_v3"
  )
  for (d in dirs) {
    dir.create(file.path(v2, d))
  }
  rd <- function(run) basename(fbc$v2_run_dir(run, v2))
  expect_identical(rd("ANC"), "ANC_aspenDispersal_logROS")
  expect_identical(rd("SprayLake"), "SprayLake_highDispersal_logROS")
  expect_identical(rd("Both"), "Both_highDispersal_logROS")
  expect_identical(fbc$v2_run_dir("FMANWT", v2), NA_character_)
  expect_identical(fbc$v2_run_dir("Trial", v2), NA_character_)
  expect_identical(fbc$v2_run_dir("ANC", NA_character_), NA_character_)
  expect_identical(fbc$v2_run_dir("ANC", file.path(v2, "missing")), NA_character_)
})
