## Alberta caribou ranges: v2 name -> v3 regional name -> ECCC name.
##   outputs/_reference/caribouAlberta3way.csv
##
## Matched by GEOMETRY (dominant overlap), because the names differ across all three and a name join
## would find almost nothing. Alberta is the interesting case: it is where v2's ECCC-derived boreal
## names were replaced by provincial ones, and where the SUBUNIT-vs-LOCALRANGE granularity question
## lives (`East Side Athabasca River` resolves into 7 subunits; every other AB range is 1:1).
##
## Two naming differences this table surfaces are NOT errors and are deliberately left alone --
## Alberta's `East/West Side Athabasca` are official enumerated domain values in its published
## metadata, and ECCC's `Snake-Sahtahneh`/`Chinchage` are the federal layer's own typos. See
## `?caribouRangeLayers` and `LandWebUtils:::.caribouNameFixes()`.
##
## RUN WITH `Rscript scripts/make_caribou_alberta_3way.R` -- NOT `--vanilla` (needs renv + Drive auth).
suppressMessages({
  pkgload::load_all("packages/LandWebUtils", quiet = TRUE)
  library(sf)
})
dp <- "inputs/caribouReference"
.rep <- function(x) spatialutils::repair_geoms(terra::vect(x))
.byName <- function(v) {
  v$Name <- trimws(v$Name); v <- v[!is.na(v$Name) & v$Name != "", ]
  terra::aggregate(v[, "Name"], by = "Name")[, "Name"]
}
aoi <- terra::aggregate(terra::project(.rep(st_read("inputs/landweb_ltfc_v10.shp", quiet=TRUE)), LandWebCRS))

## v3 regional, Alberta only
v3 <- buildCaribouRanges(aoi, destinationPath = dp)
v3 <- v3[as.data.frame(v3)$juris == "AB", ]
eco <- stats::setNames(as.data.frame(v3)$ecotype, trimws(as.data.frame(v3)$Name))
v3 <- .byName(v3)

## v2 consolidated, clipped to the AOI
v2 <- spatialutils::prep_vector(
  .byName(terra::project(.rep(st_read("inputs/Caribou_Ranges_LandWeb/caribou_landweb.shp", quiet=TRUE)), LandWebCRS)),
  aoi, crs = LandWebCRS)

## ECCC PrioritySpecies (caribou, boreal + southern mountain)
ec <- st_read(file.path(dp, "eccc", "PrioritySpecies.gdb"), quiet = TRUE)
ec <- ec[grepl("aribou", ec$CommName_E) & ec$Population_E %in% c("Boreal", "Southern Mountain"), ]
ec <- terra::project(.rep(ec), LandWebCRS)
ec$Name <- trimws(ec$Range_Name_E)
ec <- spatialutils::prep_vector(.byName(ec), aoi, crs = LandWebCRS)

## dominant overlapping name from `src` for each unit of `tgt`
dom <- function(tgt, src) {
  names(tgt) <- "t"; names(src) <- "s"
  ix <- terra::intersect(tgt, src); ix <- ix[terra::geomtype(ix) == "polygons", ]
  d <- stats::aggregate(km2 ~ t + s,
    data = data.frame(t = ix$t, s = ix$s, km2 = terra::expanse(ix, unit="km", transform=TRUE)), FUN = sum)
  at <- stats::setNames(terra::expanse(tgt, unit="km", transform=TRUE), tgt$t)
  d$pct <- d$km2 / at[d$t] * 100
  d <- d[d$pct >= 5, ]
  do.call(rbind, lapply(split(d, d$t), function(g) {
    g <- g[order(-g$pct), ]
    data.frame(t = g$t[1],
               s = paste0(g$s, " (", round(g$pct), "%)", collapse = " + "),
               stringsAsFactors = FALSE)
  }))
}
m2 <- dom(v3, v2); m3 <- dom(v3, ec)
nm <- sort(v3$Name)
tab <- data.frame(
  ecotype  = unname(eco[nm]),
  v3_AB    = nm,
  v2       = m2$s[match(nm, m2$t)],
  eccc     = m3$s[match(nm, m3$t)],
  stringsAsFactors = FALSE
)
tab$v2[is.na(tab$v2)] <- "-- none --"
tab$eccc[is.na(tab$eccc)] <- "-- none --"
tab <- tab[order(tab$ecotype, tab$v3_AB), ]
write.csv(tab, "outputs/_reference/caribouAlberta3way.csv", row.names = FALSE)
print(tab, row.names = FALSE, right = FALSE)
