## this manual must be knitted by running this script

library(bookdown)
library(knitr) ## module chapters may assume it is attached
library(SpaDES.docs)
## bibtex, data.table and RefManageR are used INSIDE SpaDES.docs, not here. RefManageR
## is only a Suggests there, so it has to stay in renv.lock (currently 1.4.0).

prjDir <- SpaDES.config::findProjectPath(from_wd = FALSE)
manDir <- file.path(prjDir, "manual") ## raw files; edit these, not the ones in `docsDir`!

## the book root is manual/, and its _bookdown.yml sets output_dir to ../docs
paths <- manualPaths(prjDir = manDir)

## bookdown writes referenced resources here; created ahead of the render so the
## directory exists whether or not this build produces figures. NOTE this is the
## PUBLISHED figures dir -- `paths$figures` is the source one (manual/figures), which
## holds the images index.Rmd references and must not be confused with it.
docsFigures <- Require::checkPath(file.path(paths$docs, "figures"), create = TRUE)

## references ---------------------------------------

writePkgBib(file.path(paths$citations, "packages.bib"))

downloadCSL("ecology-letters", paths$citations)

## references.bib is OUTPUT ONLY. It used to be its own input as well, so the manual
## accumulated into its own bibliography -- which worked locally and silently lost every
## manual-only entry on CI, where citations/ is gitignored and starts empty. The curated
## entries now live in the tracked references_manual.bib instead.
collapseModuleBibs(
  modulePath = file.path(prjDir, "modules"),
  extraBibs = file.path(paths$citations, c("packages.bib", "references_manual.bib")),
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

  ## A module with an .Rmd but no chapter in _bookdown.yml would be prepared and then
  ## left out of the book, which prepManualRmds() warns about on every build. Derive the
  ## ignore list from the chapter list rather than hand-maintaining a second copy of it:
  ## whatever _bookdown.yml does not reference is deliberately not in the manual.
  ## (Today that is HSI_Caribou_MB. To document it, add a chapter -- nothing here.)
  .chapters <- yaml::read_yaml("_bookdown.yml")$rmd_files
  .charted <- basename(sub("2\\.Rmd$", "", grep("_manual_rmds/", .chapters, value = TRUE)))
  .ignore <- setdiff(basename(list.dirs("../modules", recursive = FALSE)), .charted)

  ## NOTE: need dot because knitting is doing `rm(list = ls())`
  .copyModuleRmds <- prepManualRmds("../modules", rebuildCache = FALSE, ## use rel path!
                                    ignoreModules = .ignore)

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

  ## The archived PDFs are tracked in the repo, but the published branch is rebuilt by
  ## every deploy, so they have to be copied into docs/ or their links 404.
  ## publishManualArchive() also writes an index page built from the files actually
  ## present, which is why index.Rmd links the page instead of listing versions itself.
  publishManualArchive(
    archiveDir = file.path(manDir, "archive", "pdf"),
    docsDir = paths$docs,
    manualName = "LandWeb manual"
  )

  ## remove the temporary .Rmds, at wherever prepManualRmds() actually staged them
  unlink(unique(dirname(.copyModuleRmds)), recursive = TRUE)
})
