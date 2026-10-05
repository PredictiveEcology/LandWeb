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
## No province labels: the outlines of western Canada are recognisable without them.
bb <- sf::st_bbox(domain)
mb_xmax <- sf::st_bbox(prov[prov$NAME_1 == "Manitoba", ])[["xmax"]]
pad <- 60000
view_x <- c(bb[["xmin"]] - pad, mb_xmax + pad)
view_y <- c(bb[["ymin"]] - pad, bb[["ymax"]] + pad)
view <- sf::st_as_sfc(sf::st_bbox(
  c(xmin = view_x[1], xmax = view_x[2], ymin = view_y[1], ymax = view_y[2]),
  crs = crs
))
prov_view <- suppressWarnings(sf::st_intersection(sf::st_make_valid(prov), view))

## Size each canvas to the map's own shape so the map fills the slide's height; on a 16:9 slide
## the height runs out first, so a legend goes beside the map, not under it.
map_h <- 5.6
map_w <- map_h * diff(view_x) / diff(view_y)

base_map <- function() {
  ggplot2::geom_sf(data = prov_view, fill = "#F4F5F7", colour = "#C4CAD1", linewidth = 0.3)
}

p <- ggplot2::ggplot() +
  base_map() +
  ggplot2::geom_sf(data = domain, fill = col[["range"]], colour = NA, alpha = 0.7) +
  ggplot2::geom_sf(data = units, fill = col[["navy"]], colour = "white", linewidth = 0.15) +
  ggplot2::geom_sf(data = prov_view, fill = NA, colour = "#AEB5BD", linewidth = 0.3) +
  ggplot2::coord_sf(xlim = view_x, ylim = view_y, expand = FALSE, datum = NA) +
  theme_landweb_void() +
  ggplot2::theme(plot.margin = ggplot2::margin(2, 2, 2, 2))
save_figure(p, "landweb_units_map", width = map_w + 0.1, height = map_h + 0.1, dir = out)

## ---- fire cycle ---------------------------------------------------------------------------------
## Polygons are binned to 25-year classes (and dissolved by class, so shared edges do not show as
## hairlines); the legend is a continuous colourbar, which stays compact however many classes the
## data reach.
message(
  "v10 fire cycle (years) quantiles: ",
  paste(stats::quantile(lthfc$fri, c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)), collapse = " ")
)
bin <- 25
lthfc$fri_bin <- floor(lthfc$fri / bin) * bin + bin / 2
fc <- stats::aggregate(lthfc["fri_bin"], by = list(mid = lthfc$fri_bin), FUN = function(x) x[1])
fri_max <- ceiling(max(lthfc$fri) / bin) * bin
p <- ggplot2::ggplot() +
  base_map() +
  ggplot2::geom_sf(data = fc, ggplot2::aes(fill = mid), colour = NA) +
  ggplot2::geom_sf(data = prov_view, fill = NA, colour = "#8E959D", linewidth = 0.3) +
  ggplot2::scale_fill_gradientn(
    "Long-term\nfire cycle\n(years)",
    colours = c("#B22222", "#E8702A", "#F2C12E", "#FFF1A8"),
    limits = c(0, fri_max),
    breaks = seq(0, fri_max, by = 2 * bin),
    ## bar size set in the guide's own theme: a colourbar's length is 5 x legend.key.height, so
    ## setting the key height in the plot theme gave a 16-inch bar
    guide = ggplot2::guide_colourbar(
      theme = ggplot2::theme(
        legend.key.height = grid::unit(3.2, "in"),
        legend.key.width = grid::unit(0.3, "in")
      )
    )
  ) +
  ggplot2::coord_sf(xlim = view_x, ylim = view_y, expand = FALSE, datum = NA) +
  theme_landweb_void(base_size = 15) +
  ggplot2::theme(
    legend.position = "right",
    plot.margin = ggplot2::margin(2, 2, 2, 2)
  )
save_figure(p, "landweb_fire_cycle", width = map_w + 1.5, height = map_h + 0.1, dir = out)
