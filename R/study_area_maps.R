library(sf)

## adjust output maps_dir as needed
maps_dir <- file.path("outputs", "NW_AB_2025", "maps") |>
  fs::dir_create()

target_crs <- paste(
  "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
  "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
)

## lthfc represents the extended study area used for modelling;
## this includes a buffer to mitigate edge effects.
## lthfc is larger than the true AOI.
lthfc_ext <- file.path("inputs/LTHFC_v10_draft/lthfc_v10_draft.shp") |>
  sf::st_read(quiet = TRUE) |>
  sf::st_set_crs("epsg:26911") |>
  sf::st_transform(target_crs) |>
  dplyr::mutate(LTHFC = factor(LTFC10))

landweb_ext_area <- sf::st_union(lthfc_ext)

## drop peripheral ecozones to better reflect the true AOI
peripheral_ecozones <- c(
  "Arctic Cordillera",
  "Boreal Cordillera",
  "Hudson Plain",
  "Montane Cordillera",
  "Pacific Maritime",
  "Prairie", ## non-forest
  "Southern Arctic",
  "Taiga Cordillera"
)

ecozones <- file.path("inputs/Ecozones/ecozones.shp") |>
  sf::st_read(quiet = TRUE) |>
  dplyr::mutate(
    ecozone = tools::toTitleCase(tolower(ZONE_NAME))
  ) |>
  sf::st_transform(target_crs) |>
  sf::st_intersection(landweb_ext_area) |>
  dplyr::filter(!ecozone %in% peripheral_ecozones)

landweb_area <- sf::st_intersection(landweb_ext_area, sf::st_union(ecozones))
lthfc <- sf::st_intersection(lthfc_ext, sf::st_union(ecozones)) |>
  dplyr::group_by(LTHFC) |>
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")

landweb_bbox <- sf::st_buffer(landweb_area, 50000) |>
  sf::st_bbox() |>
  sf::st_as_sfc()

can_provs <- geodata::gadm(country = "CAN", level = 1, path = "inputs") |>
  sf::st_as_sf() |>
  sf::st_transform(target_crs) |>
  sf::st_crop(landweb_bbox) ## use bbox not area here

caribou_ranges <- file.path(
  "inputs/Caribou_Ranges_LandWeb/caribou_landweb.shp"
) |>
  sf::st_read(quiet = TRUE) |>
  sf::st_transform(target_crs) |>
  sf::st_filter(landweb_area, .predicate = sf::st_intersects) |>
  sf::st_crop(landweb_area)

## ----------------------------------------------------------------------------

## build a linear palette across all possible values (multiples of 5)
lthfc_range <- range(as.integer(as.character(lthfc$LTHFC)), na.rm = TRUE)
lthfc_all_vals <- seq(lthfc_range[1], lthfc_range[2], by = 5)
lthfc_all_cols <- setNames(
  colorRampPalette(c("red", "yellow"))(length(lthfc_all_vals)),
  as.character(lthfc_all_vals)
)
lthfc_cols <- lthfc_all_cols[levels(lthfc$LTHFC)] ## subset to only values present in data

gg_lthfc <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = can_provs, color = "black", fill = NA) +
  ggplot2::geom_sf(
    data = lthfc,
    ggplot2::aes(fill = LTHFC)
  ) +
  ggplot2::geom_sf_text(
    data = lthfc,
    ggplot2::aes(label = LTHFC),
    size = 3.5,
    # fontface = "bold",
    check_overlap = TRUE
  ) +
  ggplot2::scale_fill_manual(values = lthfc_cols) +
  ggplot2::geom_sf(
    data = landweb_area,
    color = "black",
    fill = NA,
    linewidth = 1.2
  ) +
  ggplot2::theme_bw() +
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    height = grid::unit(1.2, "cm"),
    width = grid::unit(1.2, "cm"),
    pad_x = grid::unit(0.1, "in"),
    pad_y = grid::unit(0.1, "in"),
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  ggplot2::guides(fill = "none") +
  ggplot2::ggtitle("Long-Term Historic Fire Cycle (LTFC)") +
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude")

f_gg_lthfc <- file.path(maps_dir, "landweb_lthfc.png")
ggplot2::ggsave(f_gg_lthfc, gg_lthfc, width = 16, height = 12)

## ----------------------------------------------------------------------------

gg_provs <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = can_provs, color = "black", fill = NA) +
  ggplot2::geom_sf(
    data = landweb_area,
    color = "black",
    fill = NA,
    linewidth = 1.2
  ) +
  ggplot2::theme_bw() +
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    height = grid::unit(1.2, "cm"),
    width = grid::unit(1.2, "cm"),
    pad_x = grid::unit(0.1, "in"),
    pad_y = grid::unit(0.1, "in"),
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  ggplot2::guides(fill = "none") +
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude") +
  ggplot2::ggtitle("LandWeb study area")

f_gg_provs <- file.path(maps_dir, "landweb_provinces.png")
ggplot2::ggsave(f_gg_provs, gg_provs, width = 16, height = 12)

## ----------------------------------------------------------------------------

gg_caribou <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = can_provs, color = "black", fill = NA) +
  ggplot2::geom_sf(data = caribou_ranges, fill = "steelblue", alpha = 0.5) +
  ggplot2::geom_sf(
    data = landweb_area,
    color = "black",
    fill = NA,
    linewidth = 1.2
  ) +
  ggplot2::theme_bw() +
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    height = grid::unit(1.2, "cm"),
    width = grid::unit(1.2, "cm"),
    pad_x = grid::unit(0.1, "in"),
    pad_y = grid::unit(0.1, "in"),
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude") +
  ggplot2::ggtitle("Caribou ranges within the LandWeb study area")

f_gg_caribou <- file.path(maps_dir, "landweb_caribou_ranges.png")
ggplot2::ggsave(f_gg_caribou, gg_caribou, width = 16, height = 12)

## ----------------------------------------------------------------------------

ecozone_labels <- ecozones |>
  sf::st_intersection(landweb_area) |>
  dplyr::group_by(ecozone) |>
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
  sf::st_point_on_surface()

gg_ecozones <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = ecozones, ggplot2::aes(fill = ecozone)) +
  ggplot2::geom_sf(data = can_provs, color = "black", fill = NA) +
  ggplot2::geom_sf(
    data = landweb_area,
    color = "black",
    fill = NA,
    linewidth = 1.2
  ) +
  ggplot2::geom_sf_text(
    data = ecozone_labels,
    ggplot2::aes(label = ecozone),
    size = 3.5,
    fontface = "bold",
    check_overlap = TRUE
  ) +
  ggplot2::theme_bw() +
  ggspatial::annotation_north_arrow(
    location = "bl",
    which_north = "true",
    height = grid::unit(1.2, "cm"),
    width = grid::unit(1.2, "cm"),
    pad_x = grid::unit(0.1, "in"),
    pad_y = grid::unit(0.1, "in"),
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  ggplot2::scale_fill_manual(
    guide = "none",
    values = c(
      "Atlantic Maritime" = "#B8860B",
      "Boreal Plain" = "#228B22",
      "Boreal Shield" = "#006400",
      "Mixedwood Plain" = "#DAA520",
      "Northern Arctic" = "#B0C4DE",
      "Taiga Plain" = "#8FBC8F",
      "Taiga Shield" = "#2E8B57"
    )
  ) +
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude") +
  ggplot2::ggtitle("Ecozones within the LandWeb study area")

f_gg_ecozones <- file.path(maps_dir, "landweb_ecozones.png")
ggplot2::ggsave(f_gg_ecozones, gg_ecozones, width = 16, height = 12)

## upload to Google Drive -----------------------------------------------------

## fmt: skip
googledrive::drive_auth(path = fs::dir_ls(".", type = "file", regexp = "landweb.*[.]json$"))

all_plots <- c(f_gg_caribou, f_gg_ecozones, f_gg_lthfc, f_gg_provs)
purrr::walk(.x = all_plots, .f = function(f) {
  googledrive::drive_put(
    f,
    path = googledrive::as_id("1icwggSLDnOqbAVuJCZiyIW5GrhZ8nXF0") ## maps
  )
})
