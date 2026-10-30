## Run the project's testthat suite the same way CI does (.github/workflows/tests.yaml).
##
## There is no DESCRIPTION, so nothing selects a testthat edition: TESTTHAT_EDITION=3 turns edition
## 3 on for every file, and NOT_CRAN=true keeps expect_snapshot() from skipping.
##
## R/ is NOT sourced: it holds standalone scripts, and R/project_description.R rewrites renv.lock.
## A test that needs a helper from R/ sources that file itself.
##
##   Rscript-4.6.1 scripts/run-tests.R            # everything
##   Rscript-4.6.1 scripts/run-tests.R report     # files matching a filter

Sys.setenv(TESTTHAT_EDITION = "3", NOT_CRAN = "true")
filter <- commandArgs(trailingOnly = TRUE)

testthat::test_dir(
  "tests/testthat",
  filter = if (length(filter)) filter[[1L]] else NULL,
  stop_on_failure = TRUE
)
