## this manual must be knitted by running this script

library(bibtex)
library(bookdown)
library(data.table)
library(knitr)
library(RefManageR)
library(SpaDES.docs)

prjDir <- SpaDES.config::findProjectPath(from_wd = FALSE)
manDir <- file.path(prjDir, "manual") ## raw files; edit these, not the ones in `docsDir`!

## the book root is manual/, and its _bookdown.yml sets output_dir to ../docs
paths <- manualPaths(prjDir = manDir)

## bookdown writes referenced resources here; created ahead of the render so the
## directory exists whether or not this build produces figures
Require::checkPath(file.path(paths$docs, "figures"), create = TRUE)

## references ---------------------------------------

writePkgBib(file.path(paths$citations, "packages.bib"))

downloadCSL("ecology-letters", paths$citations)

## references.bib is both an input and the output: the manual accumulates into
## its own bibliography, and every input is read before anything is written
collapseModuleBibs(
  modulePath = file.path(prjDir, "modules"),
  extraBibs = file.path(paths$citations, c("packages.bib", "references.bib")),
  outFile = file.path(paths$citations, "references.bib")
)

## RENDER BOOK ------------------------------------------

withr::with_dir(normalizePath(manDir), {
  ## set manual version. By field, not by position: read.dcf(...)[4] was Version
  ## only because it happened to be the fourth field, and any field added above
  ## it would have put the wrong string on the title page.
  Sys.setenv(LANDWEB_VERSION = read.dcf("../DESCRIPTION", fields = "Version")[1])
  Sys.getenv("LANDWEB_VERSION")

  ## don't use Require for package installation etc.
  Sys.setenv(R_USE_REQUIRE = "false")
  Sys.getenv("R_USE_REQUIRE")

  ## NOTE: need dot because knitting is doing `rm(list = ls())`
  ## HSI_Caribou_MB ships an .Rmd but has no chapter in _bookdown.yml, so it
  ## would be prepared and then left out of the book. Excluded explicitly;
  ## if it should be documented, add a chapter and drop it from here.
  .copyModuleRmds <- prepManualRmds("../modules", rebuildCache = FALSE, ## use rel path!
                                    ignoreModules = "HSI_Caribou_MB")

  ## render the book using new env -- see <https://stackoverflow.com/a/46083308>
  bookdown::render_book(output_format = "all", envir = new.env())

  ## .nojekyll has to be inside the published directory: the deploy pushes the
  ## contents of docs/, so a file at the project root never reaches the site.
  stagePagesFiles(paths$docs)

  archiveManualPDF(
    file.path(paths$docs, "LandWeb_manual.pdf"),
    version = Sys.getenv("LANDWEB_VERSION"),
    prefix = "LandWeb-manual",
    archiveDir = file.path(manDir, "archive", "pdf")
  )

  ## remove the temporary .Rmds
  unlink("_manual_rmds", recursive = TRUE)
})
