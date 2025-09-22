## this manual must be knitted by running this script

prjDir <- SpaDES.config::findProjectPath(from_wd = FALSE)

manDir <- file.path(prjDir, "manual") ## raw files; edit these, not the ones in `docsDir`!

docsDir <- file.path(manDir, "_bookdown.yml") |>
  yaml::read_yaml() |>
  purrr::pluck("output_dir") |>
  fs::path_abs()

bibDir <- file.path(manDir, "citations") |> fs::dir_create()
figDir <- file.path(docsDir, "figures") |> fs::dir_create()

# load packages -------------------------------------

library(bibtex)
library(bookdown)
library(data.table)
library(knitr)
library(RefManageR)
library(SpaDES.docs)

## references ---------------------------------------

## automatically create a bib database for R packages
allPkgs <- c("base", .packages(all.available = TRUE, lib.loc = .libPaths()))
suppressWarnings({
  ## TODO: using allPkgs, not all pkgs have dates/years
  write_bib(allPkgs, file.path(bibDir, "packages.bib"))
})

## collapse all chapter .bib files into one ------
bibFiles <- c(
  list.files(file.path(prjDir, "modules"), "references_", recursive = TRUE, full.names = TRUE),
  file.path(bibDir, "packages.bib"),
  file.path(bibDir, "references.bib")
)
bibdata <- lapply(bibFiles, function(f) {
  if (file.exists(f)) RefManageR::ReadBib(f)
})
bibdata <- Reduce(merge, bibdata)

WriteBib(bibdata, file = file.path(bibDir, "references.bib"))

csl <- file.path(bibDir, "ecology-letters.csl")
if (!file.exists(csl)) {
  download.file("https://www.zotero.org/styles/ecology-letters?source=1", destfile = csl)
}

## RENDER BOOK ------------------------------------------

withr::with_dir(normalizePath(manDir), {
  ## prevents GitHub from rendering book using Jekyll
  if (!file.exists(file.path(prjDir, ".nojekyll"))) {
    file.create(file.path(prjDir, ".nojekyll"))
  }

  ## set manual version
  Sys.setenv(LANDWEB_VERSION = read.dcf("../DESCRIPTION")[4]) ## version
  Sys.getenv("LANDWEB_VERSION")

  ## don't use Require for package installation etc.
  Sys.setenv(R_USE_REQUIRE = "false")
  Sys.getenv("R_USE_REQUIRE")

  ## NOTE: need dot because knitting is doing `rm(list = ls())`
  .copyModuleRmds <- prepManualRmds("../modules", rebuildCache = FALSE) ## use rel path!

  ## render the book using new env -- see <https://stackoverflow.com/a/46083308>
  bookdown::render_book(output_format = "all", envir = new.env())

  pdfArchiveDir <- file.path(manDir, "archive", "pdf") |> fs::dir_create()
  file.copy(
    from = file.path(docsDir, "LandWeb_manual.pdf"),
    to = file.path(pdfArchiveDir, paste0("LandWeb-manual-v", Sys.getenv("LANDWEB_VERSION"), ".pdf")),
    overwrite = TRUE
  )
  file.copy(from = dirname(pdfArchiveDir), to = docsDir, recursive = TRUE)

  ## remove temporary .Rmds
  file.remove(.copyModuleRmds)
})
