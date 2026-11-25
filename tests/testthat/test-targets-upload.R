testthat::local_edition(3)

## The upload gate and target factory (R/targets_upload.R).

skip_if_not_installed("targets")

root <- test_path("..", "..")
source(file.path(root, "R", "targets_upload.R"), local = TRUE)

name_of <- function(t) t$settings$name
command_of <- function(t) paste(deparse(t$command$expr), collapse = " ")
flat <- function(x) {
  if (inherits(x, "tar_target")) list(x) else do.call(c, lapply(x, flat))
}

test_that("the gate reads FALSE, dry or TRUE, and nothing else", {
  expect_identical(landweb_upload_mode(NULL), "FALSE")
  expect_identical(landweb_upload_mode(""), "FALSE")
  expect_identical(landweb_upload_mode("false"), "FALSE")
  expect_identical(landweb_upload_mode(FALSE), "FALSE")
  expect_identical(landweb_upload_mode("dry"), "dry")
  expect_identical(landweb_upload_mode(" DRY "), "dry")
  expect_identical(landweb_upload_mode("TRUE"), "TRUE")
  expect_identical(landweb_upload_mode(TRUE), "TRUE")
  expect_error(landweb_upload_mode("yes"), "must be FALSE, dry or TRUE")
  expect_error(landweb_upload_mode("1"), "must be FALSE, dry or TRUE")
})

test_that("with the gate off there are no upload targets", {
  expect_identical(
    upload_targets("FALSE", c("X", "Y"), extended = TRUE, n_reps = 50L, sim_end = 1000L),
    list()
  )
})

test_that("each study area gets a bundle on a worker and an upload on the main process", {
  tl <- flat(upload_targets("dry", c("X", "Y"), extended = FALSE, n_reps = 50L, sim_end = 1000L))
  names(tl) <- vapply(tl, name_of, character(1))
  expect_identical(names(tl), c("uploadBundle_X", "upload_X", "uploadBundle_Y", "upload_Y"))
  expect_identical(tl$uploadBundle_X$settings$format, "file")
  expect_identical(tl$uploadBundle_X$settings$deployment, "worker")
  expect_identical(tl$upload_X$settings$deployment, "main")
  ## the bundle depends on the report and both summaries; the upload on the bundle
  deps <- all.names(tl$uploadBundle_X$command$expr)
  expect_true(all(c("report05_X", "summaries_nrv_X_files", "summaries_burn_X_files") %in% deps))
  expect_true("uploadBundle_X" %in% all.names(tl$upload_X$command$expr))
  expect_match(command_of(tl$upload_X), landweb_drive$v3, fixed = TRUE)
  expect_match(command_of(tl$upload_X), "path = \"X\"", fixed = TRUE)
  expect_match(command_of(tl$uploadBundle_X), "out_dir = \"outputs/X/upload\"", fixed = TRUE)
})

test_that("dry and TRUE differ only in the uploads' dry_run, so the bundles are not rebuilt", {
  dry <- flat(upload_targets("dry", "X", extended = TRUE, n_reps = 50L, sim_end = 1000L))
  real <- flat(upload_targets("TRUE", "X", extended = TRUE, n_reps = 50L, sim_end = 1000L))
  names(dry) <- vapply(dry, name_of, character(1))
  names(real) <- vapply(real, name_of, character(1))
  expect_identical(names(dry), c("uploadBundle_X", "upload_X", "upload_reports"))
  expect_identical(command_of(dry$uploadBundle_X), command_of(real$uploadBundle_X))
  expect_match(command_of(dry$upload_X), "dry_run = TRUE", fixed = TRUE)
  expect_match(command_of(real$upload_X), "dry_run = FALSE", fixed = TRUE)
  expect_match(command_of(real$upload_reports), "dry_run = FALSE", fixed = TRUE)
})

test_that("each uploaded report is a _pub target the extended-analyses gate defines", {
  defined <- unlist(lapply(
    file.path(root, "R", c("targets_extended_analyses.R", "targets_report04.R")),
    function(f) all.names(parse(f))
  ))
  expect_true(all(upload_report_names %in% defined))
  expect_true(all(endsWith(upload_report_names, "_pub")))
})

test_that("the domain-wide reports go to the existing _REPORTS folder, only with the extended analyses", {
  off <- flat(upload_targets("TRUE", "X", extended = FALSE, n_reps = 50L, sim_end = 1000L))
  expect_false("upload_reports" %in% vapply(off, name_of, character(1)))
  t <- upload_reports_target(dry_run = FALSE)
  expect_identical(t$settings$deployment, "main")
  expect_true(all(upload_report_names %in% all.names(t$command$expr)))
  ## reports 00-04, 06 and 07
  expect_length(upload_report_names, 7L)
  expect_true(all(
    c("report_output_differences_pub", "report_best_practices_pub") %in% upload_report_names
  ))
  expect_match(command_of(t), "create_folders = FALSE", fixed = TRUE)
  expect_match(command_of(t), "path = \"_REPORTS\"", fixed = TRUE)
  expect_match(command_of(t), landweb_drive$drive, fixed = TRUE)
})
