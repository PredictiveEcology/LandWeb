testthat::local_edition(3)

## Page breaks in the Quarto reports under `reports/` are `{{< pagebreak >}}`, never raw `\clearpage`
## or `\newpage`, so they carry over to other formats. In LaTeX the shortcode emits `\newpage{}`,
## which leaves pending floats queued; `reports/_filters/clearpage.lua` turns it into `\clearpage`,
## which places them first. The filter matches that exact text, so a Quarto release that changes it
## would bring `\newpage` back with no error: the render test below is what notices.

root <- test_path("..", "..")
filter_path <- "reports/_filters/clearpage.lua"

reports <- list.files(file.path(root, "reports"), pattern = "[.]qmd$", full.names = TRUE)

test_that("there are reports to check", {
  expect_gt(length(reports), 0L)
})

test_that("_quarto.yml runs the clearpage filter after Quarto's own filters", {
  filters <- yaml::read_yaml(file.path(root, "_quarto.yml"))$filters
  entry <- Filter(function(f) identical(f$path, filter_path), filters)
  expect_length(entry, 1L)
  expect_identical(entry[[1]]$at, "post-quarto")
})

test_that("{{< pagebreak >}} renders to \\clearpage in LaTeX, also inside a div", {
  quarto <- Sys.which("quarto")
  skip_if_not(nzchar(quarto), "quarto is not installed")

  ## A minimal project carrying only the real `filters:` entry and the real filter file, so the
  ## render needs nothing outside the repository.
  proj <- withr::local_tempdir()
  dir.create(file.path(proj, dirname(filter_path)), recursive = TRUE)
  file.copy(file.path(root, filter_path), file.path(proj, filter_path))
  yaml::write_yaml(
    list(
      project = list(type = "default"),
      filters = yaml::read_yaml(file.path(root, "_quarto.yml"))$filters
    ),
    file.path(proj, "_quarto.yml")
  )
  writeLines(
    c("One.", "", "{{< pagebreak >}}", "", "::: {.callout-note}", "Two.", "", "{{< pagebreak >}}", ":::"),
    file.path(proj, "x.qmd")
  )

  withr::local_dir(proj)
  status <- system2(quarto, c("render", "x.qmd", "--to", "latex", "--quiet"))
  expect_identical(status, 0L)
  tex <- readLines("x.tex")
  expect_identical(sum(tex == "\\clearpage"), 2L)
  expect_false(any(grepl("\\newpage", tex, fixed = TRUE)))
})

for (report in reports) {
  lines <- readLines(report, warn = FALSE, encoding = "UTF-8")

  test_that(paste(basename(report), "breaks pages with the shortcode, not raw LaTeX"), {
    expect_identical(which(grepl("\\\\(clearpage|newpage)", lines)), integer(0))
  })
}
