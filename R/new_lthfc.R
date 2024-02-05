library(dplyr)
library(ggplot2)
library(googledrive)
library(sf)
library(terra)

## add polygons to existing LTHFC map ---------------------------------------------------------
v_old <- "v8b"
v_new <- "v8c"

lthfc_old <- file.path("inputs", paste0("landweb_ltfc_", v_old, ".shp")) |>
  st_read(quiet = TRUE) |>
  st_set_crs("epsg:26911")
ecodistricts <- file.path("inputs", "ecodistricts.shp") |>
  st_read(quiet = TRUE) |>
  st_transform(st_crs(lthfc_old))

polys2add <- ecodistricts[ ecodistricts$ECODISTRIC %in% c(183, 270, 271, 272), ] ## northern MB
polys2add <- polys2add[, colnames(polys2add) %in% colnames(lthfc_old)]
polys2add$LTHFC <- 80L

lthfc_new <- bind_rows(lthfc_old, polys2add)

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
