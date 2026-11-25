testthat::local_edition(3)

## drive_sync() and its planner (R/drive_sync.R), against a fake Drive: no googledrive, no network.

root <- test_path("..", "..")
source(file.path(root, "R", "drive_sync.R"), local = TRUE)

sa_email <- "landweb@landweb-343704.iam.gserviceaccount.com"

## A Drive in memory. `api` has the functions drive_sync() calls; `state$items` is what Drive holds
## (the destination folder has id "DEST"), and `state$calls` names every call made. With
## `lose_first_upload`, the first upload stores its file and then fails, as when Drive's reply is
## lost.
fake_drive <- function(user = sa_email, lose_first_upload = FALSE) {
  state <- new.env()
  state$items <- data.frame(
    id = "DEST",
    parent = NA_character_,
    name = "v3",
    md5 = NA_character_,
    mime_type = drive_folder_mime,
    stringsAsFactors = FALSE
  )
  state$calls <- character()
  state$n <- 0L
  state$lost <- FALSE
  add <- function(parent, name, md5, mime_type) {
    state$n <- state$n + 1L
    id <- sprintf("id%03d", state$n)
    state$items <- rbind(
      state$items,
      data.frame(
        id = id,
        parent = parent,
        name = name,
        md5 = md5,
        mime_type = mime_type,
        stringsAsFactors = FALSE
      )
    )
    id
  }
  log <- function(what) state$calls <- c(state$calls, what)
  api <- list(
    login = function() {
      log("login")
      invisible(TRUE)
    },
    user = function() user,
    ls = function(id) {
      log("ls")
      state$items[state$items$parent %in% id, c("name", "id", "md5", "mime_type")]
    },
    mkdir = function(name, parent) {
      log("mkdir")
      add(parent, name, NA_character_, drive_folder_mime)
    },
    upload = function(file, parent, name) {
      log("upload")
      id <- add(parent, name, unname(tools::md5sum(file)), "application/octet-stream")
      if (lose_first_upload && !state$lost) {
        state$lost <- TRUE
        stop("connection reset")
      }
      id
    },
    update = function(id, file) {
      log("update")
      state$items$md5[state$items$id == id] <- unname(tools::md5sum(file))
      id
    },
    retry = function(fn) {
      for (i in 1:3) {
        res <- tryCatch(fn(), error = function(e) e)
        if (!inherits(res, "error")) {
          return(res)
        }
      }
      stop(res)
    }
  )
  list(api = api, state = state)
}

## files under a temporary root: 05-X.pdf, figures/a.png and figures/lm/b.png
local_bundle <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(dir, "figures", "lm"), recursive = TRUE)
  writeLines("pdf", file.path(dir, "05-X.pdf"))
  writeLines("a", file.path(dir, "figures", "a.png"))
  writeLines("b", file.path(dir, "figures", "lm", "b.png"))
  dir
}
bundle_files <- function(dir) {
  file.path(dir, c("05-X.pdf", "figures/a.png", "figures/lm/b.png"))
}

writes <- c("mkdir", "upload", "update")

## ---- drive_sync_plan() --------------------------------------------------------------------------

remote_row <- function(dir, name, id, md5 = NA_character_, mime_type = "application/pdf") {
  data.frame(dir = dir, name = name, id = id, md5 = md5, mime_type = mime_type)
}

test_that("new files are uploaded, unchanged skipped, changed updated in place", {
  local <- data.frame(
    path = c("a.csv", "b.csv", "c.csv", "sub/d.png"),
    md5 = c("m1", "m2", "m3new", "m4")
  )
  remote <- rbind(
    remote_row("", "b.csv", "idB", "m2"),
    remote_row("", "c.csv", "idC", "m3old"),
    remote_row("", "old.csv", "idOld", "m9")
  )
  plan <- drive_sync_plan(local, remote)
  expect_identical(plan$path, c("sub", "a.csv", "b.csv", "c.csv", "sub/d.png"))
  expect_identical(plan$type, c("folder", rep("file", 4L)))
  expect_identical(plan$action, c("create", "upload", "skip", "update", "upload"))
  expect_identical(plan$drive_id, c(NA, NA, "idB", "idC", NA))
})

test_that("a plan never deletes: a file only on Drive is left out of it", {
  plan <- drive_sync_plan(
    data.frame(path = "a.csv", md5 = "m1"),
    rbind(remote_row("", "a.csv", "idA", "m1"), remote_row("", "gone.csv", "idG", "m2"))
  )
  expect_false("gone.csv" %in% plan$path)
  expect_true(all(plan$action %in% c("create", "exists", "upload", "update", "skip")))
})

test_that("folders already on Drive are reused and their files compared", {
  remote <- rbind(
    remote_row("", "sub", "idSub", mime_type = drive_folder_mime),
    remote_row("sub", "d.png", "idD", "m4"),
    ## same name, other folder: not a match
    remote_row("", "d.png", "idTop", "m4")
  )
  plan <- drive_sync_plan(
    data.frame(path = c("sub/d.png", "sub/e.png"), md5 = c("m4", "m5")),
    remote
  )
  expect_identical(plan$action, c("exists", "skip", "upload"))
  expect_identical(plan$drive_id, c("idSub", "idD", NA))
})

test_that("two files or folders of one name on Drive stop the plan", {
  dup_files <- rbind(remote_row("", "a.csv", "id1", "m1"), remote_row("", "a.csv", "id2", "m1"))
  expect_error(
    drive_sync_plan(data.frame(path = "a.csv", md5 = "m1"), dup_files),
    "2 files named 'a.csv'"
  )
  dup_dirs <- rbind(
    remote_row("", "sub", "id1", mime_type = drive_folder_mime),
    remote_row("", "sub", "id2", mime_type = drive_folder_mime)
  )
  expect_error(
    drive_sync_plan(data.frame(path = "sub/a.csv", md5 = "m1"), dup_dirs),
    "2 folders named 'sub'"
  )
})

test_that("a Google-native file is not replaced", {
  remote <- remote_row("", "notes", "idN", mime_type = "application/vnd.google-apps.document")
  expect_error(
    drive_sync_plan(data.frame(path = "notes", md5 = "m1"), remote),
    "Google-native"
  )
})

test_that("destination paths must be relative and clean, and each file named once", {
  for (p in c("/a.csv", "../a.csv", "a//b.csv", "./a.csv", "a/")) {
    expect_error(drive_sync_plan(data.frame(path = p, md5 = "m"), NULL), "invalid destination")
  }
  expect_error(
    drive_sync_plan(data.frame(path = c("a.csv", "a.csv"), md5 = c("m1", "m2")), NULL),
    "more than one local file"
  )
  expect_identical(
    nrow(drive_sync_plan(data.frame(path = character(), md5 = character()), NULL)),
    0L
  )
})

## ---- drive_sync() -------------------------------------------------------------------------------

test_that("a dry run lists and plans but writes nothing", {
  dir <- local_bundle()
  fd <- fake_drive()
  before <- fd$state$items
  plan <- drive_sync(bundle_files(dir), dir, "DEST", path = "X", dry_run = TRUE, api = fd$api)
  expect_identical(fd$state$items, before)
  expect_false(any(fd$state$calls %in% writes))
  expect_identical(
    plan$path,
    c("X", "X/figures", "X/figures/lm", "X/05-X.pdf", "X/figures/a.png", "X/figures/lm/b.png")
  )
  expect_identical(plan$action, c(rep("create", 3L), rep("upload", 3L)))
  expect_true(all(plan$dry_run))
})

test_that("a sync uploads the tree, and a second sync of the same files writes nothing", {
  dir <- local_bundle()
  fd <- fake_drive()
  first <- drive_sync(bundle_files(dir), dir, "DEST", path = "X", dry_run = FALSE, api = fd$api)
  expect_identical(sum(fd$state$calls == "mkdir"), 3L)
  expect_identical(sum(fd$state$calls == "upload"), 3L)
  expect_false(anyNA(first$drive_id))
  ## each file is in its folder on Drive, under its own name
  items <- fd$state$items
  lm <- items$id[items$name == "lm"]
  expect_identical(items$parent[items$name == "b.png"], lm)
  expect_identical(items$parent[items$name == "X"], "DEST")

  fd$state$calls <- character()
  second <- drive_sync(bundle_files(dir), dir, "DEST", path = "X", dry_run = FALSE, api = fd$api)
  expect_false(any(fd$state$calls %in% writes))
  expect_identical(second$action, c(rep("exists", 3L), rep("skip", 3L)))
  expect_identical(second$drive_id, first$drive_id)
  ## one listing per folder
  expect_identical(sum(fd$state$calls == "ls"), 4L)
})

test_that("a changed file is updated in place and a file gone locally stays on Drive", {
  dir <- local_bundle()
  fd <- fake_drive()
  first <- drive_sync(bundle_files(dir), dir, "DEST", path = "X", dry_run = FALSE, api = fd$api)
  writeLines("a, redrawn", file.path(dir, "figures", "a.png"))
  fd$state$calls <- character()
  files <- bundle_files(dir)[1:2] ## figures/lm/b.png no longer sent
  second <- drive_sync(files, dir, "DEST", path = "X", dry_run = FALSE, api = fd$api)
  expect_identical(fd$state$calls[fd$state$calls %in% writes], "update")
  a <- second$path == "X/figures/a.png"
  expect_identical(second$action[a], "update")
  expect_identical(second$drive_id[a], first$drive_id[first$path == "X/figures/a.png"])
  expect_true("b.png" %in% fd$state$items$name)
  expect_identical(
    fd$state$items$md5[fd$state$items$name == "a.png"],
    unname(tools::md5sum(file.path(dir, "figures", "a.png")))
  )
})

test_that("a retried upload whose first attempt reached Drive leaves one copy", {
  dir <- local_bundle()
  fd <- fake_drive(lose_first_upload = TRUE)
  plan <- drive_sync(bundle_files(dir), dir, "DEST", dry_run = FALSE, api = fd$api)
  expect_identical(sum(fd$state$items$name == "05-X.pdf"), 1L)
  expect_identical(
    plan$drive_id[plan$path == "05-X.pdf"],
    fd$state$items$id[fd$state$items$name == "05-X.pdf"]
  )
})

test_that("only the service account may sync, and it is checked before Drive is listed", {
  dir <- local_bundle()
  fd <- fake_drive(user = "someone@example.org")
  expect_error(
    drive_sync(bundle_files(dir), dir, "DEST", dry_run = TRUE, api = fd$api),
    "not as the LandWeb service account"
  )
  expect_identical(fd$state$calls, "login")
  expect_error(drive_check_service_account("someone@gmail.com"), "someone@gmail.com")
  expect_error(drive_check_service_account(NA_character_), "as nobody")
  expect_error(drive_check_service_account(NULL), "as nobody")
  expect_error(drive_check_service_account("iam.gserviceaccount.com@example.org"), "refusing")
  expect_identical(drive_check_service_account(sa_email), sa_email)
})

test_that("create_folders = FALSE stops before writing when a folder is missing", {
  dir <- local_bundle()
  fd <- fake_drive()
  expect_error(
    drive_sync(
      bundle_files(dir)[1L],
      dir,
      "DEST",
      path = "_REPORTS",
      dry_run = FALSE,
      create_folders = FALSE,
      api = fd$api
    ),
    "no folder _REPORTS"
  )
  expect_false(any(fd$state$calls %in% writes))
  ## with the folder there, the file goes into it
  rid <- fd$api$mkdir("_REPORTS", "DEST")
  plan <- drive_sync(
    bundle_files(dir)[1L],
    dir,
    "DEST",
    path = "_REPORTS",
    dry_run = FALSE,
    create_folders = FALSE,
    api = fd$api
  )
  expect_identical(plan$action, c("exists", "upload"))
  expect_identical(fd$state$items$parent[fd$state$items$name == "05-X.pdf"], rid)
})

test_that("files must exist and lie below root", {
  dir <- local_bundle()
  other <- withr::local_tempfile(lines = "x")
  fd <- fake_drive()
  expect_error(drive_sync(other, dir, "DEST", api = fd$api), "not below")
  expect_error(drive_sync(file.path(dir, "nope.pdf"), dir, "DEST", api = fd$api), "not files")
  expect_length(fd$state$calls, 0L)
})

test_that("the real Drive functions have the fake's names, and a dribble reads as a listing", {
  ## building the list calls nothing on Drive
  expect_named(drive_api_googledrive(key = "unused"), names(fake_drive()$api))
  dribble <- data.frame(name = c("a.png", "sub"), id = c("i1", "i2"))
  dribble$drive_resource <- list(
    list(md5Checksum = "m1", mimeType = "image/png"),
    list(mimeType = drive_folder_mime)
  )
  expect_identical(
    drive_dribble_frame(dribble),
    data.frame(
      name = c("a.png", "sub"),
      id = c("i1", "i2"),
      md5 = c("m1", NA),
      mime_type = c("image/png", drive_folder_mime)
    )
  )
})
