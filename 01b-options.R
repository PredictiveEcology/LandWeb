.ncores <- min(parallelly::availableCores(), 24L)

options(
  Ncpus = .ncores,
  repos = c(CRAN = "https://cloud.r-project.org")
)
