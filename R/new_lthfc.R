library(dplyr)
library(ggplot2)
library(googledrive)
library(sf)
library(terra)

## add polygons to existing LTHFC map ---------------------------------------------------------
v_old <- "v8c"
v_new <- "v8d"

lthfc_old <- file.path("inputs", paste0("landweb_ltfc_", v_old, ".shp")) |>
  st_read(quiet = TRUE) |>
  st_set_crs("epsg:26911")

fmas <- file.path("inputs", "FMA_Boundary_Updated_2024.shp") |>
  st_read(quiet = TRUE) |>
  st_transform(st_crs(lthfc_old)) |>
  st_make_valid()
fmas <- fmas[grep("Spray Lake|Crowsnest", fmas$Name), ]

sls <- file.path("inputs", "ltfc_sls_v3.shp") |>
  st_read(quiet = TRUE) |>
  st_set_crs("epsg:26911") |>
  st_make_valid()

## TODO: only want to update LTHFCs for these two FMAs
# updated <- rbind(
#   sls[apply(st_intersects(sls, fmas[1, ], sparse = FALSE), 1, any), ],
#   sls[apply(st_intersects(sls, fmas[2, ], sparse = FALSE), 1, any), ]
# )

ggplot(st_crop(sls, fmas)) +
  geom_sf(aes(fill = LTHFC)) +
  geom_sf(data = fmas, fill = "orange")

lthfc_new <- st_join(lthfc_old, updated)
lthfc_new$LTHFC <- lthfc_new$LTHFC.x
lthfc_new$LTHFC[!is.na(lthfc_new$LTHFC.y)] <- lthfc_new$LTHFC.y[!is.na(lthfc_new$LTHFC.y)]
lthfc_new$LTHFC.x <- NULL
lthfc_new$LTHFC.y <- NULL

lthfc_new$NAME <- lthfc_new$NAME.x
lthfc_new$NAME.x <- NULL
lthfc_new$NAME.y <- NULL

lthfc_new <- relocate(lthfc_new, NAME, LTHFC, .before = ZONE_NOM_)

## inspect + confirm differences --------------------------------------------------------------
gg_old <- ggplot(lthfc_old) +
  geom_sf(aes(fill = LTHFC))

gg_new <- ggplot(lthfc_new) +
  geom_sf(aes(fill = LTHFC))

cowplot::plot_grid(gg_old, gg_new)

r <- rast(lthfc_new, ncols = 3000, nrows = 2000)
r_old <- rasterize(lthfc_old, r, "LTHFC")
r_new <- rasterize(lthfc_new, r, "LTHFC")
r_diff <- sum(-r_old, r_new, na.rm = TRUE)

plot(r_diff)

## save to disk and archive -------------------------------------------------------------------
st_write(lthfc_new, file.path("inputs", paste0("landweb_ltfc_", v_new, ".shp")))

zip_new <- file.path("inputs", paste0("landweb_ltfc_", v_new, ".zip"))
withr::with_dir("inputs", {
  archive::archive_write_files(
    archive = basename(zip_new),
    files = list.files(".", pattern = paste0("landweb_ltfc_", v_new)) |>
      grep("[.]zip$", x = _, invert = TRUE, value = TRUE)
  )
})

## upload the new zip file --------------------------------------------------------------------
gid <- drive_put(zip_new, as_id("1LsYuuYICkcpElAkEABFM5zJXf5tTyMLG"))

gid$id ## use this drive file id in the preamble module
