colorVec <- diff(c(ageClassCutOffs[1], 10, 30, 50, ageClassCutOffs[-(1:4)], maxAge))

timeSinceFirePalette <- leaflet::colorNumeric(
  na.color = "transparent",
  c(rep("red", colorVec[1]), rep("orange", colorVec[2]), rep("yellow", colorVec[3]),
    paste0(colorRampPalette(c("light green", "dark green"))(colorVec[4]), "FF")),
  domain = NULL
)

colorTableFile <- file.path("www", "color_table.txt")
checkPath(dirname(colorTableFile), create = TRUE)
color_tableFn <- function(timeSinceFirePalette, maxAge) {
  a <- t(sapply(timeSinceFirePalette(1:maxAge), col2rgb))
  rownames(a) <- NULL
  a <- rbind(rep(0,4), cbind(1:maxAge, a))
  write.table(a, file = colorTableFile, append = FALSE, row.names = FALSE, col.names = FALSE)
}
color_tableFn(timeSinceFirePalette, maxAge)
