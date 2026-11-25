## One-way sync of local files to a Google Drive folder, for the upload targets (R/targets_upload.R).
##
## drive_sync_plan() decides, without touching Drive, what to do with each file: a file whose name
## and md5 match the one file of that name on Drive is skipped, a changed file is updated in place
## (so its Drive id and links stay the same), a new file is uploaded, and a missing folder is created.
## Nothing on Drive is ever deleted, so a file removed locally stays there until removed by hand.
##
## drive_sync() lists each destination folder once, plans, and carries out the plan. It logs in as
## the LandWeb service account and stops unless Drive confirms that account, so a pipeline run never
## uploads under a person's login. Every Drive call goes through `api` (drive_api_googledrive() by
## default), so the tests run against a fake Drive with neither googledrive nor a network.

drive_folder_mime <- "application/vnd.google-apps.folder"

#' Plan a one-way sync of local files to a Drive folder
#'
#' @param local Data frame of the files to sync, one row each: `path`, the file's path below the
#'   destination folder ("/"-separated, e.g. `"figures/lm/a.png"`), and `md5`. Any other columns
#'   (such as `local`, the file on disk) are carried into the plan.
#' @param remote Data frame of what is on Drive below the destination, one row per item, as listed
#'   by [drive_list_tree()]: `dir` (the folder holding the item, relative to the destination, `""` for
#'   the destination itself), `name`, `id`, `md5` (`NA` for folders and Google-native files) and
#'   `mime_type`.
#'
#' @return Data frame, one row per folder the files need and then one per file: `path`, `type`
#'   (`"folder"` or `"file"`), `action` (`"create"` or `"exists"` for a folder; `"upload"`,
#'   `"update"` or `"skip"` for a file), `drive_id` (`NA` where Drive has no such item yet), `md5`,
#'   and the extra columns of `local`. Never a deletion.
drive_sync_plan <- function(local, remote) {
  local <- as.data.frame(local, stringsAsFactors = FALSE)
  if (!all(c("path", "md5") %in% names(local))) {
    stop("drive_sync_plan(): `local` needs columns path and md5", call. = FALSE)
  }
  bad <- local$path[!drive_valid_path(local$path)]
  if (length(bad)) {
    stop("drive_sync_plan(): invalid destination path(s): ", toString(bad), call. = FALSE)
  }
  dup <- unique(local$path[duplicated(local$path)])
  if (length(dup)) {
    stop("drive_sync_plan(): more than one local file for: ", toString(dup), call. = FALSE)
  }
  remote <- drive_remote_frame(remote)
  is_folder <- remote$mime_type %in% drive_folder_mime

  ## every folder above a file, parents before children
  dirs <- unique(as.character(unlist(lapply(dirname_drive(local$path), drive_ancestors))))
  dirs <- dirs[order(lengths(strsplit(dirs, "/", fixed = TRUE)), dirs)]
  folder_id <- c("." = "") ## "" is the destination itself; drive_sync() supplies its id
  folders <- lapply(dirs, function(d) {
    parent <- dirname_drive(d)
    name <- basename(d)
    ## a parent being created has nothing in it yet
    hit <- if (identical(folder_id[[parent_key(parent)]], NA_character_)) {
      remote[0, ]
    } else {
      remote[is_folder & remote$dir == parent & remote$name == name, ]
    }
    if (nrow(hit) > 1L) {
      stop(
        "drive_sync_plan(): ",
        nrow(hit),
        " folders named '",
        d,
        "' on Drive; remove all but one by hand",
        call. = FALSE
      )
    }
    id <- if (nrow(hit)) hit$id[[1L]] else NA_character_
    folder_id[[parent_key(d)]] <<- id
    data.frame(
      path = d,
      type = "folder",
      action = if (is.na(id)) "create" else "exists",
      drive_id = id,
      md5 = NA_character_,
      stringsAsFactors = FALSE
    )
  })

  files <- lapply(seq_len(nrow(local)), function(i) {
    p <- local$path[[i]]
    parent <- dirname_drive(p)
    hit <- if (identical(folder_id[[parent_key(parent)]], NA_character_)) {
      remote[0, ]
    } else {
      remote[!is_folder & remote$dir == parent & remote$name == basename(p), ]
    }
    if (nrow(hit) > 1L) {
      stop(
        "drive_sync_plan(): ",
        nrow(hit),
        " files named '",
        p,
        "' on Drive; remove all but one by hand",
        call. = FALSE
      )
    }
    if (nrow(hit) && isTRUE(startsWith(hit$mime_type[[1L]], "application/vnd.google-apps."))) {
      stop(
        "drive_sync_plan(): '",
        p,
        "' is a Google-native file on Drive and cannot be replaced by a local file",
        call. = FALSE
      )
    }
    action <- if (!nrow(hit)) {
      "upload"
    } else if (identical(hit$md5[[1L]], local$md5[[i]])) {
      "skip"
    } else {
      "update"
    }
    data.frame(
      path = p,
      type = "file",
      action = action,
      drive_id = if (nrow(hit)) hit$id[[1L]] else NA_character_,
      md5 = local$md5[[i]],
      stringsAsFactors = FALSE
    )
  })

  plan <- do.call(rbind, c(folders, files))
  if (is.null(plan)) {
    plan <- data.frame(
      path = character(),
      type = character(),
      action = character(),
      drive_id = character(),
      md5 = character(),
      stringsAsFactors = FALSE
    )
  }
  extra <- setdiff(names(local), names(plan))
  for (col in extra) {
    plan[[col]] <- local[[col]][match(plan$path, local$path)]
  }
  rownames(plan) <- NULL
  plan
}

#' List what is on Drive below a folder, one listing per folder
#'
#' Lists `folder` and then, for each of `dirs` that exists on Drive, that subfolder. A folder that
#' does not exist, or that Drive holds more than once, is not listed: [drive_sync_plan()] creates
#' the first and stops on the second.
#'
#' @param folder Drive id of the destination folder.
#' @param dirs Subfolder paths below `folder` ("/"-separated) whose contents are needed; their
#'   parents are added.
#' @param api Drive functions, as from [drive_api_googledrive()]; only `ls` and `retry` are used.
#'
#' @return Data frame with columns `dir`, `name`, `id`, `md5` and `mime_type`.
drive_list_tree <- function(folder, dirs, api) {
  dirs <- unique(as.character(unlist(lapply(dirs, drive_ancestors))))
  dirs <- dirs[order(lengths(strsplit(dirs, "/", fixed = TRUE)), dirs)]
  listing <- function(id, dir) {
    out <- drive_remote_frame(api$retry(function() api$ls(id)))
    out$dir <- rep(dir, nrow(out))
    out
  }
  found <- list(listing(folder, ""))
  for (d in dirs) {
    seen <- do.call(rbind, found)
    hit <- seen[
      seen$mime_type %in%
        drive_folder_mime &
        seen$dir == dirname_drive(d) &
        seen$name == basename(d),
    ]
    if (nrow(hit) == 1L) {
      found[[length(found) + 1L]] <- listing(hit$id[[1L]], d)
    }
  }
  out <- do.call(rbind, found)
  rownames(out) <- NULL
  out
}

#' Sync local files to a Drive folder
#'
#' Uploads `files` to the Drive folder `folder` (below its subfolder `path`), keeping their layout
#' below `root`. Lists each destination folder once; skips files whose name and md5 match Drive;
#' updates changed files in place, keeping their Drive ids; uploads new ones; creates missing
#' folders. Never deletes anything on Drive. Each Drive call is retried (`api$retry`); an upload or
#' folder creation that is retried first checks whether the failed attempt reached Drive, so a retry
#' does not leave two copies.
#'
#' @param files Local paths of the files to sync.
#' @param root Local directory the files are below; their paths below it are their paths on Drive.
#' @param folder Drive id of the destination folder.
#' @param path Character vector of subfolder names below `folder` to sync into (`"BigTroutLake"`,
#'   say), or `character()` for `folder` itself.
#' @param dry_run If `TRUE`, list and plan but write nothing to Drive.
#' @param create_folders If `FALSE`, stop rather than create a folder that is not on Drive yet
#'   (for a destination that must already exist).
#' @param api Drive functions; see [drive_api_googledrive()].
#' @param account_pattern Regular expression the logged-in account must match.
#'
#' @return The plan from [drive_sync_plan()] with the Drive id of every item, a `local` column (the
#'   file), `dry_run`, and `time` (when the sync finished).
drive_sync <- function(
  files,
  root,
  folder,
  path = character(),
  dry_run = TRUE,
  create_folders = TRUE,
  api = drive_api_googledrive(),
  account_pattern = "[.]iam[.]gserviceaccount[.]com$"
) {
  files <- as.character(files)
  missing <- files[!file.exists(files) | dir.exists(files)]
  if (length(missing)) {
    stop("drive_sync(): not files: ", toString(missing), call. = FALSE)
  }
  rel <- drive_relative_paths(files, root)
  dest <- if (length(path)) paste(paste(path, collapse = "/"), rel, sep = "/") else rel
  local <- data.frame(
    path = dest,
    md5 = unname(tools::md5sum(files)),
    local = files,
    stringsAsFactors = FALSE
  )

  api$login()
  drive_check_service_account(api$user(), account_pattern)

  remote <- drive_list_tree(folder, dirname_drive(dest), api)
  plan <- drive_sync_plan(local, remote)
  to_create <- plan$path[plan$type == "folder" & plan$action == "create"]
  if (!create_folders && length(to_create)) {
    stop(
      "drive_sync(): no folder ",
      toString(to_create),
      " on Drive below ",
      folder,
      ", and create_folders = FALSE; check the folder and the service account's access to it",
      call. = FALSE
    )
  }

  if (!isTRUE(dry_run)) {
    ids <- c("." = folder)
    for (i in seq_len(nrow(plan))) {
      parent <- ids[[parent_key(dirname_drive(plan$path[[i]]))]]
      name <- basename(plan$path[[i]])
      plan$drive_id[[i]] <- switch(
        plan$action[[i]],
        exists = plan$drive_id[[i]],
        skip = plan$drive_id[[i]],
        create = drive_once(api, parent, name, NA_character_, function() api$mkdir(name, parent)),
        upload = drive_once(
          api,
          parent,
          name,
          plan$md5[[i]],
          function() api$upload(plan$local[[i]], parent, name)
        ),
        update = api$retry(function() api$update(plan$drive_id[[i]], plan$local[[i]]))
      )
      if (plan$type[[i]] == "folder") {
        ids[[parent_key(plan$path[[i]])]] <- plan$drive_id[[i]]
      }
    }
  }
  plan$dry_run <- rep(isTRUE(dry_run), nrow(plan))
  plan$time <- rep(Sys.time(), nrow(plan))
  plan
}

#' Stop unless Drive reports a service account
#'
#' @param email The logged-in account's address (`googledrive::drive_user()$emailAddress`).
#' @param pattern Regular expression a service-account address matches.
#'
#' @return `email`, invisibly.
drive_check_service_account <- function(email, pattern = "[.]iam[.]gserviceaccount[.]com$") {
  if (length(email) != 1L || is.na(email) || !nzchar(email) || !grepl(pattern, email)) {
    stop(
      "drive_sync(): logged in to Drive as ",
      if (length(email) == 1L && !is.na(email) && nzchar(email)) email else "nobody",
      ", not as the LandWeb service account; refusing to upload. ",
      "GOOGLEDRIVE_AUTH must name the service-account key (LandWeb.Renviron, see .Rprofile).",
      call. = FALSE
    )
  }
  invisible(email)
}

#' The Drive functions drive_sync() uses, on the real Drive
#'
#' Each element is a function: `login()` logs in with the service-account key only (never a cached
#' personal token); `user()` returns the logged-in address; `ls(id)` lists a folder (via
#' `workflowtools::drive_ls_folder()`, which works for an account shared into a shared drive without
#' being a member of it) as a data frame of `name`, `id`, `md5`, `mime_type`; `mkdir(name, parent)`,
#' `upload(file, parent, name)` and `update(id, file)` return the item's Drive id; `retry(fn)` runs
#' `fn()` with `workflowtools::with_retry()`.
#'
#' @param key Path to the service-account key; by default `GOOGLEDRIVE_AUTH`, which `.Rprofile`
#'   sets from `LandWeb.Renviron`.
#'
#' @return A named list of functions.
drive_api_googledrive <- function(key = Sys.getenv("GOOGLEDRIVE_AUTH")) {
  list(
    login = function() {
      if (!nzchar(key)) {
        stop(
          "drive_sync(): GOOGLEDRIVE_AUTH is not set, so there is no service-account key to log in ",
          "with (LandWeb.Renviron, see .Rprofile)",
          call. = FALSE
        )
      }
      gargle::with_cred_funs(
        list(credentials_service_account = gargle::credentials_service_account),
        googledrive::drive_auth(path = key)
      )
      invisible(TRUE)
    },
    user = function() {
      u <- googledrive::drive_user()
      if (is.null(u)) NA_character_ else u$emailAddress %||% NA_character_
    },
    ls = function(id) drive_dribble_frame(workflowtools::drive_ls_folder(googledrive::as_id(id))),
    mkdir = function(name, parent) {
      as.character(googledrive::drive_mkdir(name, path = googledrive::as_id(parent))$id)
    },
    upload = function(file, parent, name) {
      as.character(
        googledrive::drive_upload(file, path = googledrive::as_id(parent), name = name)$id
      )
    },
    update = function(id, file) {
      as.character(googledrive::drive_update(googledrive::as_id(id), media = file)$id)
    },
    retry = function(fn) workflowtools::with_retry(fn, times = 3L, wait = 10)
  )
}

## A dribble as the data frame drive_sync_plan() reads.
drive_dribble_frame <- function(d) {
  res <- d$drive_resource
  pick <- function(field) {
    vapply(res, function(r) r[[field]] %||% NA_character_, character(1))
  }
  data.frame(
    name = as.character(d$name),
    id = as.character(d$id),
    md5 = pick("md5Checksum"),
    mime_type = pick("mimeType"),
    stringsAsFactors = FALSE
  )
}

## Create an item once: on a retry, first look for what the failed attempt may have left on Drive
## (an item of that name, and for a file that md5), so that retrying does not leave a duplicate.
drive_once <- function(api, parent, name, md5, create) {
  attempt <- 0L
  api$retry(function() {
    attempt <<- attempt + 1L
    if (attempt > 1L) {
      have <- drive_remote_frame(api$ls(parent))
      same <- if (is.na(md5)) have$mime_type %in% drive_folder_mime else have$md5 %in% md5
      hit <- have[have$name == name & same, ]
      if (nrow(hit) == 1L) {
        return(hit$id[[1L]])
      }
    }
    create()
  })
}

## `remote` with the columns drive_sync_plan() reads, all character.
drive_remote_frame <- function(remote) {
  cols <- c("dir", "name", "id", "md5", "mime_type")
  if (is.null(remote) || !nrow(remote)) {
    out <- as.data.frame(
      stats::setNames(rep(list(character()), length(cols)), cols),
      stringsAsFactors = FALSE
    )
    return(out)
  }
  remote <- as.data.frame(remote, stringsAsFactors = FALSE)
  for (col in cols) {
    remote[[col]] <- if (col %in% names(remote)) as.character(remote[[col]]) else NA_character_
  }
  remote[cols]
}

## Paths of `files` below `root`, "/"-separated; stops on a file outside `root`.
drive_relative_paths <- function(files, root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  abs <- normalizePath(files, winslash = "/", mustWork = TRUE)
  inside <- startsWith(abs, paste0(root, "/"))
  if (!all(inside)) {
    stop("drive_sync(): not below ", root, ": ", toString(files[!inside]), call. = FALSE)
  }
  substring(abs, nchar(root) + 2L)
}

## A destination path is relative, "/"-separated, with no empty, "." or ".." part.
drive_valid_path <- function(p) {
  vapply(
    strsplit(p, "/", fixed = TRUE),
    function(parts) length(parts) > 0L && all(nzchar(parts) & !parts %in% c(".", "..")),
    logical(1)
  ) &
    !startsWith(p, "/") &
    !endsWith(p, "/")
}

## dirname() of a "/"-separated destination path, "" at the top.
dirname_drive <- function(p) {
  d <- dirname(p)
  d[d == "."] <- ""
  d
}

## The folder `d` and every folder above it, top first; nothing for "".
drive_ancestors <- function(d) {
  if (!nzchar(d)) {
    return(character())
  }
  parts <- strsplit(d, "/", fixed = TRUE)[[1L]]
  vapply(seq_along(parts), function(i) paste(parts[seq_len(i)], collapse = "/"), character(1))
}

## Name of a folder in a lookup vector: "." for the destination itself.
parent_key <- function(d) if (nzchar(d)) d else "."
