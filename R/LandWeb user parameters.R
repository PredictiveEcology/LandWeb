eventCaching <- c(".inputObjects", "init")
maxAge <- 400
vegLeadingProportion <- 0.8 # indicates what proportion the stand must be in one species group for it to be leading.
# If all are below this, then it is a "mixed" stand
ageClasses <- c("Young", "Immature", "Mature", "Old")
ageClassCutOffs <- c(0, 40, 80, 120)
ageClassZones <- lapply(seq_along(ageClassCutOffs), function(x) {
  if (x < length(ageClassCutOffs)) {
    paste0(ageClassCutOffs[x], "-", ageClassCutOffs[x + 1])
  } else {
    paste0(">", ageClassCutOffs[x])
  }
})

