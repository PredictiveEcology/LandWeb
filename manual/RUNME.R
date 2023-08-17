## This manual must be knitted by running this script

if (file.exists("~/.Renviron")) readRenviron("~/.Renviron") ## GITHUB_PAT

## packages -----------------------------------------

prjDir <- "~/GitHub/LandWeb"
manDir <- file.path(prjDir, "manual")
figDir <- Require::checkPath(file.path(prjDir, "figures"), create = TRUE)

options(
  Ncpus = min(parallel::detectCores() / 2, 8)
)

# load packages -------------------------------------

library("bibtex")
library("bookdown")
library("data.table")
library("knitr")
library("RefManageR")
library("SpaDES.docs")

## references ---------------------------------------

## automatically create a bib database for R packages
bibDir <- Require::checkPath(file.path(manDir, "citations"), create = TRUE)
allPkgs <- .packages(all.available = TRUE, lib.loc = .libPaths()[1]) ## TODO: some pkg desc files broken
keyPkgs <- c(
  "bookdown", "knitr", "LandR", "LandWebUtils",
  "reproducible", "rmarkdown", "shiny", "SpaDES.core", "SpaDES.tools"
)
write_bib(keyPkgs, file.path(bibDir, "packages.bib")) ## TODO: use allPkgs

## collapse all chapter .bib files into one ------
bibFiles <- c(
  list.files(file.path(prjDir, "m"), "references_", recursive = TRUE, full.names = TRUE),
  file.path(bibDir, "packages.bib"),
  file.path(bibDir, "references.bib")
)
bibdata <- lapply(bibFiles, function(f) {
  if (file.exists(f)) RefManageR::ReadBib(f)
})
bibdata <- Reduce(merge, bibdata)

WriteBib(bibdata, file = file.path(bibDir, "references.bib"))

if (!file.exists(file.path(bibDir, "ecology-letters.csl"))) {
  download.file("https://www.zotero.org/styles/ecology-letters?source=1",
                destfile = "citations/ecology-letters.csl")
}

## RENDER BOOK ------------------------------------------

setwd(normalizePath(manDir))

## prevents GitHub from rendering book using Jekyll
if (!file.exists(file.path(manDir, ".nojekyll"))) {
  file.create(file.path(manDir, ".nojekyll"))
}

## set manual version
Sys.setenv(LANDWEB_MAN_VERSION = read.dcf("../DESCRIPTION")[3]) ## version

## don't use Require for package installation etc.
Sys.setenv(R_USE_REQUIRE = "false")

## render the book using new env -- see <https://stackoverflow.com/a/46083308>

## NOTE: need dot because knitting is doing `rm(list = ls())`
.copyModuleRmds <- prepManualRmds("../m", rebuildCache = FALSE) ## use rel path!

bookdown::render_book(output_format = "all", envir = new.env())
# bookdown::render_book(output_format = "bookdown::pdf_book", envir = new.env())
# bookdown::render_book(output_format = "bookdown::bs4_book", envir = new.env())

pdfArchiveDir <- checkPath(file.path("archive", "pdf"), create = TRUE)
file.copy(from = file.path(manDir, "LandRManual.pdf"),
          to = file.path(pdfArchiveDir, paste0("LandR-manual-v", Sys.getenv("LANDR_MAN_VERSION"), ".pdf")),
          overwrite = TRUE)

## remove temporary .Rmds
file.remove(.copyModuleRmds)
setwd(prjDir)
