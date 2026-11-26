## mainSim_<sa> branches over the stem rep_index. With retrieval left at "auto", targets loads each
## branch's upstream values in the controller (dataPrep's cohortData: 40-130 MB serialized per task)
## and sends a copy with every task, so a burst of dispatches fills the controller's memory cap.
## With retrieval = "worker" the workers read those values from the shared store themselves.

root <- test_path("..", "..")

## every `retrieval = <value>` passed to tar_option_set() anywhere in a file, in order
retrieval_settings <- function(path) {
  found <- character()
  walk <- function(e) {
    if (is.call(e)) {
      fn <- e[[1L]]
      is_set <- identical(fn, quote(tar_option_set)) ||
        identical(fn, quote(targets::tar_option_set))
      if (is_set && "retrieval" %in% names(e)) {
        found[[length(found) + 1L]] <<- deparse(e[["retrieval"]])
      }
      for (a in as.list(e)[-1L]) {
        walk(a)
      }
    }
  }
  for (e in parse(path, keep.source = FALSE)) {
    walk(e)
  }
  found
}

test_that("_targets.R makes workers load upstream values from the store", {
  settings <- retrieval_settings(file.path(root, "_targets.R"))
  expect_gte(length(settings), 1L)
  expect_identical(unique(settings), "\"worker\"")
})
