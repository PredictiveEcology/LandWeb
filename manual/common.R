## clean workspace
rm(list = ls(all.names = FALSE))

# example R options set globally
options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  width = 60
)

## knitr-related options
options(
  knitr.graphics.rel_path = FALSE,
  knitr.table.format = function() {
    if (knitr::is_latex_output())
      "latex" else "pipe"
  }
)

# chunk options set globally
knitr::opts_chunk$set(
  fig.pos = "H",
  out.extra = "",
  size = "tiny",
  tidy = TRUE,
  tidy.opts = list(width.cutoff = 60)
)
