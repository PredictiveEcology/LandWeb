testthat::local_edition(3)

## report05_age_class_labels() names each age class with the ages it holds. The age classes are
## closed on the right (nrvtools:::.age_class_raster()), so a stand exactly at a cutoff is in the
## class below it: at LandWeb's cutoffs, age 40 is young and age 41 immature.

root <- test_path("..", "..")
source(file.path(root, "R", "report05_helpers.R"), local = TRUE)

lw_classes <- c("Young", "Immature", "Mature", "Old")
lw_cutoffs <- c(0L, 40L, 80L, 120L)

test_that("at LandWeb's cutoffs, each cutoff is the top of the class below it", {
  expect_identical(
    report05_age_class_labels(lw_classes, lw_cutoffs),
    c(
      "Young (0 to 40 years)",
      "Immature (41 to 80 years)",
      "Mature (81 to 120 years)",
      "Old (over 120 years)"
    )
  )
})

test_that("the ranges follow the cutoffs given", {
  expect_identical(
    report05_age_class_labels(c("Early", "Mid", "Late"), c(10, 60, 150)),
    c("Early (10 to 60 years)", "Mid (61 to 150 years)", "Late (over 150 years)")
  )
  expect_identical(
    report05_age_class_labels(c("Young", "Old"), c(0L, 100L)),
    c("Young (0 to 100 years)", "Old (over 100 years)")
  )
})

test_that("a cutoff for each class, and at least two classes, are required", {
  expect_error(report05_age_class_labels(lw_classes, lw_cutoffs[-4L]), "one cutoff for each")
  expect_error(report05_age_class_labels("Old", 0L), "at least two")
})

test_that("the labels state the ages nrvtools puts in each class", {
  skip_if_not_installed("terra")
  skip_if_not_installed("nrvtools")
  ages <- 0:200
  r <- terra::rast(matrix(ages, nrow = 1L))
  got <- terra::values(nrvtools:::.age_class_raster(r, lw_cutoffs, lw_classes), mat = FALSE)
  ## each label's range, "a to b years" or "over a years", as its youngest and oldest whole-year age
  lab <- report05_age_class_labels(lw_classes, lw_cutoffs)
  pat <- "([0-9]+) to ([0-9]+) years|over ([0-9]+) years"
  m <- do.call(rbind, regmatches(lab, regexec(pat, lab)))
  over <- nzchar(m[, 4L])
  lo <- ifelse(over, as.numeric(m[, 4L]) + 1, as.numeric(m[, 2L]))
  hi <- ifelse(over, Inf, as.numeric(m[, 3L]))
  ## vapply() fails on an age that no label, or two labels, cover
  stated <- vapply(ages, function(a) which(a >= lo & a <= hi), integer(1))
  expect_identical(as.integer(got), stated)
})
