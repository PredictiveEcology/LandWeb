testthat::local_edition(3)

## Report 04's data builders (R/report04_data.R), on small synthetic inputs written in the shape of
## the v2 summary files.

skip_if_not_installed("data.table")

root <- test_path("..", "..")
r04 <- new.env()
for (f in c("fma_boundary_change.R", "report_data.R", "report04_data.R")) {
  sys.source(file.path(root, "R", f), envir = r04)
}

## ---- v2 summary files -----------------------------------------------------------------------

## A v2 `boxplots/leading_<layer>.csv` for two polygons and two simulated landscapes: fir has 1, 2, 3
## and 4 pixels in the four age classes, black spruce twice that, and all forest is their sum. In
## polygon "B", fir is absent from the second landscape (totalPixels 0, proportion NaN).
write_v2_leading <- function(run_dir) {
  grid <- expand.grid(
    ageClass = c("Young", "Immature", "Mature", "Old"),
    vegCover = c("All species", "Fir", "Bl Spruce"),
    group = c("rep01_year0700._.Lay One", "rep02_year1000._.Lay One"),
    zone = c("A", "B"),
    stringsAsFactors = FALSE
  )
  base <- c(Young = 1, Immature = 2, Mature = 3, Old = 4)[grid$ageClass]
  fir_absent <- grid$zone == "B" & grepl("^rep02", grid$group)
  fir <- ifelse(fir_absent, 0, base)
  grid$NPixels <- 0
  grid$NPixels[grid$vegCover == "Fir"] <- fir[grid$vegCover == "Fir"]
  grid$NPixels[grid$vegCover == "Bl Spruce"] <- 2 * base[grid$vegCover == "Bl Spruce"]
  is_all <- grid$vegCover == "All species"
  grid$NPixels[is_all] <- fir[is_all] + 2 * base[is_all]
  tot <- stats::ave(grid$NPixels, grid$zone, grid$group, grid$vegCover, FUN = sum)
  grid$totalPixels <- tot
  grid$proportion <- ifelse(tot > 0, round(grid$NPixels / tot, 4), NaN)
  grid$proportionCC <- 0.25
  grid$label <- "x"
  grid$totalPixels2 <- tot
  cols <- c(
    "zone",
    "vegCover",
    "ageClass",
    "proportionCC",
    "group",
    "NPixels",
    "proportion",
    "label",
    "totalPixels",
    "totalPixels2"
  )
  dir.create(file.path(run_dir, "boxplots"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(grid[cols], file.path(run_dir, "boxplots", "leading_Lay_One.csv"))
  invisible(grid)
}

test_that("read_v2_leading() keeps one polygon, parses landscapes, and drops absent types", {
  run <- withr::local_tempdir()
  write_v2_leading(run)
  a <- r04$read_v2_leading(run, "Lay One", "A")
  ## polygon B's rows must not leak in (both polygons share the same keys)
  expect_identical(nrow(a$nrv), 3L * 4L * 2L)
  expect_setequal(a$nrv$rep, c(1L, 2L))
  expect_setequal(a$nrv$time, c(700L, 1000L))
  expect_setequal(a$nrv$vegType, c("All species", "Fir", "Bl_Spruce"))
  expect_equal(sum(a$nrv[vegType == "Fir" & rep == 1L]$value), 1, tolerance = 1e-3)
  expect_identical(nrow(a$cc), 3L * 4L)
  b <- r04$read_v2_leading(run, "Lay One", "B")
  expect_false(any(b$nrv$vegType == "Fir" & b$nrv$rep == 2L))
  expect_true(all(b$nrv$type_pixels > 0))
  expect_error(r04$read_v2_leading(run, "Lay One", "C"), "no rows for v2 polygon 'C'")
  expect_error(r04$read_v2_leading(run, "Other", "A"), "no v2 age-class file")
})

test_that("v2_type_share() gives each type's share of forest, 0 where it is absent", {
  nrv <- data.table::data.table(
    vegType = c("All species", "Fir", "Pine", "All species", "Pine"),
    ageClass = "Old",
    rep = c(1L, 1L, 1L, 2L, 2L),
    time = 700L,
    value = 1,
    n_pixels = 1,
    type_pixels = c(10, 4, 6, 8, 8)
  )
  s <- r04$v2_type_share(nrv)
  data.table::setkey(s, rep, vegType)
  expect_identical(nrow(s), 4L)
  expect_equal(s$value, c(0.4, 0.6, 0, 1))
  expect_equal(s[, sum(value), by = rep]$V1, c(1, 1))
})

write_v2_patches <- function(run_dir, size, rows) {
  dir.create(file.path(run_dir, "histograms"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    rows,
    file.path(run_dir, "histograms", paste0("largePatches_Lay_One_", size, ".csv"))
  )
}

test_that("read_v2_patches() fills landscapes without patches with 0 and keeps the current value", {
  run <- withr::local_tempdir()
  row <- function(rep, area, cc, poly = "A", age = "Old", veg = "All species") {
    data.frame(
      ageClass = age,
      polygonName = poly,
      vegCover = veg,
      rep = rep,
      N = 1,
      totalArea = area,
      NCC = 1,
      totalAreaCC = cc
    )
  }
  write_v2_patches(
    run,
    100,
    rbind(
      row(1, 500, 300),
      row(3, 700, 300),
      row(2, 999, 999, poly = "B"),
      row(2, 5, 5, age = "Young")
    )
  )
  write_v2_patches(run, 500, row(2, 999, 999, poly = "B"))
  p <- r04$read_v2_patches(run, "Lay One", "A", sizes = c(100L, 500L), n_snapshots = 4L)
  expect_identical(p$nrv[min_patch_ha == 100L]$value, c(500, 0, 700, 0))
  expect_identical(p$nrv[min_patch_ha == 500L]$value, rep(0, 4L))
  expect_identical(p$cc$value, c(300, 0))
  expect_error(
    r04$read_v2_patches(run, "Lay One", "A", sizes = 100L, n_snapshots = 2L),
    "more landscapes"
  )
})

test_that("the v2 INFO.md model settings are read, and nothing about the machine", {
  run <- withr::local_tempdir()
  expect_length(r04$.v2_info_md(run), 0L)
  writeLines(
    c(
      "# Run info",
      "",
      "| ------------- | ---- |",
      "|       machine | node.example.org |",
      "|          user | someone |",
      "| dispersalType |  high |",
      "|     pixelSize |   250 |",
      "| ------------- | ---- |",
      "```r",
      "$`Git repository`"
    ),
    file.path(run, "INFO.md")
  )
  expect_identical(r04$.v2_info_md(run), c(dispersalType = "high", pixelSize = "250"))
})

test_that("v2 labels map onto the v3 reporting codes", {
  expect_identical(
    r04$.v2_veg_type(c("All species", "Bl Spruce", "Wh Spruce", "Doug fir", "Decid")),
    c("All species", "Bl_Spruce", "Wh_Spruce", "Doug_fir", "Decid")
  )
  expect_identical(r04$.v2_snapshot("rep03_year0750._.X Y"), list(rep = 3L, time = 750L))
  expect_error(r04$.v2_snapshot("CC._.X"), "unexpected v2 snapshot labels")
})

## ---- comparison ---------------------------------------------------------------------------------

test_that("report04_compare() puts both versions' percentiles side by side", {
  nrv <- data.table::data.table(
    tenure = "T",
    vegType = "All species",
    ageClass = "Old",
    version = rep(c("v2", "v3"), c(3L, 5L)),
    value = c(0.1, 0.2, 0.3, 0.3, 0.4, 0.5, 0.6, 0.7)
  )
  cc <- data.table::data.table(
    tenure = "T",
    vegType = "All species",
    ageClass = "Old",
    version = c("v2", "v3"),
    value = c(0.05, 0.5)
  )
  x <- r04$report04_compare(nrv, cc, c("tenure", "vegType", "ageClass"))
  expect_identical(nrow(x$long), 2L)
  expect_identical(x$long[version == "v3"]$n, 5L)
  expect_identical(x$long[version == "v2"]$position, "below")
  d <- x$delta
  expect_identical(nrow(d), 1L)
  expect_equal(d$p50_v2, 0.2)
  expect_equal(d$p50_v3, 0.5)
  expect_equal(d$d_p50, 0.3)
  expect_equal(d$d_cc, 0.45)
})

test_that("report04_compare() leaves out groups that only one version has", {
  nrv <- data.table::data.table(
    tenure = "T",
    vegType = c("Fir", "Fir", "Pine"),
    ageClass = "Old",
    version = c("v2", "v3", "v3"),
    value = 0.5
  )
  cc <- nrv[, .(tenure, vegType, ageClass, version, value = 0.4)]
  x <- r04$report04_compare(nrv, cc, c("tenure", "vegType", "ageClass"))
  expect_identical(nrow(x$long), 3L)
  expect_identical(x$delta$vegType, "Fir")
})

## ---- current-condition maps -------------------------------------------------------------------

test_that("v2_cc_composition() counts forest with a type and an age inside the polygon", {
  skip_if_not_installed("terra")
  run <- withr::local_tempdir()
  ## a 4 x 4 grid of 1 m cells in the LandWeb projection; the polygon covers the left half (8 cells)
  lcc <- r04$.fma_target_crs()
  vtm <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4, crs = lcc)
  terra::values(vtm) <- c(1, 1, 2, 2, 1, NA, 2, 2, 3, 3, 2, 2, 3, 1, 2, 2)
  levels(vtm) <- data.frame(ID = 1:3, VALUE = c("Abie_sp", "Pinu_sp", "Popu_sp"))
  tsf <- terra::rast(vtm)
  terra::values(tsf) <- c(10, 50, 0, 0, 130, 10, 0, 0, 90, NA, 0, 0, 30, 41, 0, 0)
  terra::writeRaster(vtm, file.path(run, "CurrentConditionVTM.tif"))
  terra::writeRaster(tsf, file.path(run, "CurrentConditionTSF.tif"))
  poly <- terra::as.polygons(terra::ext(0, 2, 0, 4), crs = lcc)
  x <- r04$v2_cc_composition(
    run,
    poly,
    c("Young", "Immature", "Mature", "Old"),
    c(0L, 40L, 80L, 120L)
  )
  ## left half: fir at ages 10, 50, 130, 41; aspen at 90, 30 (one aspen cell has no age, one cell no type)
  expect_identical(x$share$vegType, c("Decid", "Fir"))
  expect_equal(x$share$value, c(2 / 6, 4 / 6))
  fir <- x$by_age[vegType == "Fir"]
  expect_equal(fir[match(c("Young", "Immature", "Old"), ageClass)]$value, c(0.25, 0.5, 0.25))
  all <- x$by_age[vegType == "All species"]
  expect_equal(sum(all$value), 1)
})
