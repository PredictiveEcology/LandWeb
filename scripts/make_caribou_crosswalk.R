## Build the v2 -> v3 caribou reporting-unit crosswalk:
##   outputs/_reference/caribouCrosswalk.csv  -- one row per v2 unit x overlapping v3 unit
##
## WHY THIS EXISTS: v3 assembles caribou ranges from the six jurisdictional sources
## (`LandWebUtils::buildCaribouRanges()`), where v2 labelled its boreal units from ECCC's national
## layer -- `Bistcho (BIS)`, `Saskatchewan Boreal Plains (BPL)`, and so on. The ground barely moved,
## but the UNIT NAMES changed, and reporting units are keyed by name (`refCode` is slugged from it).
## So a v2<->v3 comparison cannot match on name; it has to match on geometry. This produces that
## mapping once, so nobody re-derives it by eye.
##
## v2 SOURCE: `inputs/Caribou_Ranges_LandWeb/caribou_landweb.shp`, the consolidated layer v2 used for
## nearly every study area. NB the v2 preamble ALSO added the Manitoba and Saskatchewan provincial
## layers for those study areas specifically (`provMB.R` pulled `ml[["MB Caribou Ranges"]]` where
## every other area pulled `ml[["LandWeb Caribou Ranges"]]`). v3 uses those provincial layers directly,
## so for MB/SK study areas the v2->v3 mapping is 1:1 by name and is not the interesting part; the
## crosswalk below is against the consolidated layer, which is where the renaming actually happened.
##
## RUN WITH `Rscript scripts/make_caribou_crosswalk.R` -- NOT `--vanilla` (needs renv + Drive auth).
## Study-area-INDEPENDENT, so it lives at the outputs/ root under `_reference/` (see CLAUDE.md).
suppressMessages({
  pkgload::load_all("packages/LandWebUtils", quiet = TRUE)
  library(sf)
})

dp <- reproducible::checkPath("inputs/caribouReference", create = TRUE)
outDir <- reproducible::checkPath("outputs/_reference", create = TRUE)

## NEVER sf::st_make_valid(): it collapses reversed ring-winding-order polygons. terra re-orients them.
.repair <- function(x) spatialutils::repair_geoms(terra::vect(x))
.km2 <- function(x) as.numeric(sum(sf::st_area(sf::st_as_sf(x)))) / 1e6

## one multipolygon per NAME -- reporting units are keyed by name, and a unit split into disjoint
## parts is tallied as one unit downstream, so the crosswalk must compare names, not features
.byName <- function(v) {
  v$Name <- trimws(v$Name)
  v <- v[!is.na(v$Name) & v$Name != "", ]
  ## aggregate() adds its own `agg_n` count column; drop it so the layer carries `Name` alone and can
  ## be renamed for the intersect below
  terra::aggregate(v[, "Name"], by = "Name")[, "Name"]
}

aoi <- .repair(sf::st_read("inputs/landweb_ltfc_v10.shp", quiet = TRUE)) |>
  terra::project(LandWebCRS) |>
  terra::aggregate()

message("loading the v2 consolidated caribou layer ...")
v2 <- .repair(sf::st_read("inputs/Caribou_Ranges_LandWeb/caribou_landweb.shp", quiet = TRUE)) |>
  terra::project(LandWebCRS)
v2 <- .byName(v2)
v2 <- spatialutils::prep_vector(v2, aoi, crs = LandWebCRS)

message("assembling the v3 caribou layer ...")
v3 <- buildCaribouRanges(aoi, destinationPath = dp)
v3$Name <- trimws(v3$Name)
v3j <- stats::setNames(as.data.frame(v3)$juris, as.data.frame(v3)$Name) ## Name -> jurisdiction
v3 <- .byName(v3)

## Capture the name vectors BEFORE the columns are renamed for the intersect below. (Reading them
## afterwards via `v2$Name` silently yields NULL, which made `setdiff()` return character(0) and
## reported "no retired, no added units" regardless of the truth.)
nm2 <- v2$Name
nm3 <- v3$Name
a2 <- stats::setNames(terra::expanse(v2, unit = "km", transform = TRUE), nm2)
a3 <- stats::setNames(terra::expanse(v3, unit = "km", transform = TRUE), nm3)

## ---- pairwise overlap ---------------------------------------------------------------------------
## One `terra::intersect()` over the whole layers, NOT a nested loop: 78 x 69 pairwise intersects is
## ~5,400 calls, where the single call returns every overlapping pair with both attribute sets.
message("intersecting ", nrow(v2), " v2 units x ", nrow(v3), " v3 units ...")
names(v2) <- "v2"
names(v3) <- "v3"
ix <- terra::intersect(v2, v3)
ix <- ix[terra::geomtype(ix) == "polygons", ]
ov <- data.frame(
  v2 = ix$v2, v3 = ix$v3,
  km2 = terra::expanse(ix, unit = "km", transform = TRUE),
  stringsAsFactors = FALSE
)
ov <- stats::aggregate(km2 ~ v2 + v3, data = ov, FUN = sum)
## a shared edge yields a sliver, not a correspondence
ov <- ov[ov$km2 > 1, ]
ov$pct_of_v2 <- ov$km2 / a2[ov$v2] * 100
ov$pct_of_v3 <- ov$km2 / a3[ov$v3] * 100
## count a correspondence only where it accounts for >=5% of EITHER side
ov <- ov[ov$pct_of_v2 >= 5 | ov$pct_of_v3 >= 5, ]

n2 <- table(ov$v2) ## how many v3 units each v2 unit maps onto
n3 <- table(ov$v3) ## and vice versa

## ---- classify -----------------------------------------------------------------------------------
## Deliberately reported per PAIR rather than collapsed to one verdict per v2 unit: a split is only
## legible if you can see the parts and their shares.
.rel <- function(v2n, v3n, p2, p3) {
  many2 <- n2[[v2n]] > 1L
  many3 <- n3[[v3n]] > 1L
  if (many2 && many3) return("reorganised")   ## m:n -- boundaries genuinely redrawn
  if (many2) return("split")                  ## one v2 unit -> several v3 units
  if (many3) return("merged")                 ## several v2 units -> one v3 unit
  ## 1:1 -- distinguish a pure rename from a boundary change, and identity from either
  if (p2 >= 98 && p3 >= 98) {
    if (identical(v2n, v3n)) "identical" else "renamed"
  } else {
    "boundary-change"
  }
}
ov$relation <- vapply(seq_len(nrow(ov)), function(i) {
  .rel(ov$v2[i], ov$v3[i], ov$pct_of_v2[i], ov$pct_of_v3[i])
}, character(1))

## v2 units with no v3 counterpart, and vice versa. `retired` is expected and explicable -- the ECCC
## national ranges v2 labelled its boreal units from, now replaced by provincial names; and Banff,
## which v3 drops as locally extirpated.
retired <- setdiff(nm2, ov$v2)
added <- setdiff(nm3, ov$v3)

out <- rbind(
  data.frame(
    v2_unit = ov$v2, v3_unit = ov$v3, v3_juris = unname(v3j[ov$v3]),
    relation = ov$relation, overlap_km2 = round(ov$km2),
    pct_of_v2 = round(ov$pct_of_v2, 1), pct_of_v3 = round(ov$pct_of_v3, 1),
    stringsAsFactors = FALSE
  ),
  if (length(retired)) data.frame(
    v2_unit = retired, v3_unit = NA_character_, v3_juris = NA_character_,
    relation = "retired", overlap_km2 = 0L,
    pct_of_v2 = 0, pct_of_v3 = NA_real_, stringsAsFactors = FALSE
  ),
  if (length(added)) data.frame(
    v2_unit = NA_character_, v3_unit = added, v3_juris = unname(v3j[added]),
    relation = "added", overlap_km2 = 0L,
    pct_of_v2 = NA_real_, pct_of_v3 = 0, stringsAsFactors = FALSE
  )
)
out <- out[order(out$relation, out$v2_unit, -out$pct_of_v2), ]
utils::write.csv(out, file.path(outDir, "caribouCrosswalk.csv"), row.names = FALSE)

cat("\nv2 units:", nrow(v2), "| v3 units:", nrow(v3), "| crosswalk rows:", nrow(out), "\n\n")
print(table(out$relation))
cat("\nv2 units with NO v3 counterpart (", length(retired), "):\n  ",
    paste(sort(retired), collapse = ", "), "\n", sep = "")
cat("\nv3 units with NO v2 counterpart (", length(added), "):\n  ",
    paste(sort(added), collapse = ", "), "\n", sep = "")
cat("\nwrote: ", file.path(outDir, "caribouCrosswalk.csv"), "\n")
