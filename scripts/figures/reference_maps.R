## Reference maps for LandWeb talks and the manual.
##
## Writes PNG to fig_dir() (default manual/figures). No SVG: the polygon maps come to ~7 MB each.
##   landweb_units_map   -- the LandWeb domain (the extent of the v10 fire-cycle map) and the
##                          forest management units simulated in BC, AB, SK, MB and NWT.
##                          Ontario tenures are omitted: they sit inside the v3 study-area
##                          groups but Ontario is not a LandWeb partner.
##   landweb_fire_cycle  -- the v10 long-term historic fire cycle (years).
##
## Inputs (built by the pipeline; see R/lthfc_summary.R and scripts/make_sa_reference.R):
##   outputs/_extended_analyses/lthfc_change_map.gpkg  layers `lthfc` (v8c + v10), `provinces`
##   outputs/_reference/studyAreaGroups.gpkg            layer `members`
## Run on a compute node from the project root: Rscript-4.6.1 scripts/figures/reference_maps.R

if (!exists("theme_landweb")) {
  source("scripts/figures/theme.R")
}

out <- fig_dir()
col <- landweb_colours
lthfc_gpkg <- "outputs/_extended_analyses/lthfc_change_map.gpkg"
sa_gpkg <- "outputs/_reference/studyAreaGroups.gpkg"

lthfc <- sf::st_read(lthfc_gpkg, layer = "lthfc", quiet = TRUE)
lthfc <- lthfc[grepl("^v10", lthfc$version), ]
crs <- sf::st_crs(lthfc)
domain <- sf::st_union(sf::st_make_valid(lthfc))
prov <- sf::st_read(lthfc_gpkg, layer = "provinces", quiet = TRUE)

members <- sf::st_transform(sf::st_read(sa_gpkg, layer = "members", quiet = TRUE), crs)
is_on <- members$province == "ON"
units <- members[!is_on, ]
units_area_km2 <- as.numeric(sf::st_area(sf::st_union(units))) / 1e6
message(sprintf(
  "units shown: %d of %d members (%d Ontario omitted); union area %s km2",
  nrow(units),
  nrow(members),
  sum(is_on),
  format(round(units_area_km2), big.mark = ",")
))

## View: the whole domain west to east as far as just past Manitoba. The domain continues into
## northwestern Ontario, which is not a partner jurisdiction, so the map stops at the MB border.
bb <- sf::st_bbox(domain)
mb_xmax <- sf::st_bbox(prov[prov$NAME_1 == "Manitoba", ])[["xmax"]]
pad <- 60000
view_x <- c(bb[["xmin"]] - 4 * pad, mb_xmax + pad)
view_y <- c(bb[["ymin"]] - pad, bb[["ymax"]] + pad)
view <- sf::st_as_sfc(sf::st_bbox(
  c(xmin = view_x[1], xmax = view_x[2], ymin = view_y[1], ymax = view_y[2]),
  crs = crs
))
prov_view <- suppressWarnings(sf::st_intersection(sf::st_make_valid(prov), view))

## Province labels at hand-placed lon/lat points clear of the management units.
prov_lab <- sf::st_as_sf(
  data.frame(
    abbr = c("BC", "AB", "SK", "MB", "NWT"),
    lon = c(-126.5, -111.8, -105.5, -97.5, -118.5),
    lat = c(56.5, 58.2, 56.8, 57.0, 62.3)
  ),
  coords = c("lon", "lat"),
  crs = 4326
) |>
  sf::st_transform(crs)

base_map <- function() {
  ggplot2::geom_sf(data = prov_view, fill = "#F4F5F7", colour = "#C4CAD1", linewidth = 0.3)
}
prov_labels <- function() {
  ggplot2::geom_sf_text(
    data = prov_lab,
    ggplot2::aes(label = abbr),
    family = "Carlito",
    size = 14 / ggplot2::.pt,
    colour = col[["muted"]],
    fontface = "bold"
  )
}

p <- ggplot2::ggplot() +
  base_map() +
  ggplot2::geom_sf(data = domain, fill = col[["range"]], colour = NA, alpha = 0.7) +
  ggplot2::geom_sf(data = units, fill = col[["navy"]], colour = "white", linewidth = 0.15) +
  ggplot2::geom_sf(data = prov_view, fill = NA, colour = "#AEB5BD", linewidth = 0.3) +
  prov_labels() +
  ggplot2::coord_sf(xlim = view_x, ylim = view_y, expand = FALSE, datum = NA) +
  theme_landweb_void()
save_figure(p, "landweb_units_map", width = 7.6, height = 5.7, dir = out)

## ---- fire cycle ---------------------------------------------------------------------------------
message(
  "v10 fire cycle (years) quantiles: ",
  paste(stats::quantile(lthfc$fri, c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)), collapse = " ")
)
brks <- c(0, 25, 50, 75, 100, 150, Inf)
labs <- c("< 25", "25-50", "50-75", "75-100", "100-150", "150+")
lthfc$fri_class <- cut(lthfc$fri, brks, labels = labs, right = FALSE)
## dissolve by class so shared polygon edges do not show as hairlines
fc <- stats::aggregate(
  lthfc["fri_class"],
  by = list(cls = lthfc$fri_class),
  FUN = function(x) x[1]
)
ramp <- grDevices::colorRampPalette(c("#B22222", "#E8702A", "#F2C12E", "#FFF1A8"))(length(labs))
p <- ggplot2::ggplot() +
  base_map() +
  ggplot2::geom_sf(data = fc, ggplot2::aes(fill = cls), colour = NA) +
  ggplot2::geom_sf(data = prov_view, fill = NA, colour = "#8E959D", linewidth = 0.3) +
  prov_labels() +
  ggplot2::scale_fill_manual(
    "Fire cycle (years)",
    values = stats::setNames(ramp, labs),
    drop = FALSE,
    guide = ggplot2::guide_legend(nrow = 1, title.position = "top")
  ) +
  ggplot2::coord_sf(xlim = view_x, ylim = view_y, expand = FALSE, datum = NA) +
  theme_landweb_void(base_size = 14) +
  ggplot2::theme(legend.key.width = ggplot2::unit(0.35, "in"))
save_figure(p, "landweb_fire_cycle", width = 6.3, height = 5.7, dir = out)
