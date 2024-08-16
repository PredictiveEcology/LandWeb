library(dplyr)
library(ggplot2)
library(googledrive)
library(sf)
library(terra)

## add polygons to existing FMA map -----------------------------------------------------------
fma_curr <- file.path("inputs", "FMA_Boundary_Updated.shp") |>
  st_read(quiet = TRUE) |>
  st_zm()
fma_curr$OBJECTID_1 <- rownames(fma_curr)

fma_addl <- file.path("inputs", "FMA_Boundaries", "AB_FMAs_2022-03-25", "Forest Management Agreement Area.shp") |>
  st_read(quiet = TRUE) |>
  st_zm() |>
  st_transform(st_crs(fma_curr)) |>
  rename(Name = FMA_NAME)

polys2add <- fma_addl[ fma_addl$Name == "Crowsnest Forest Products Ltd.", ] ## C5
polys2add <- polys2add[, colnames(polys2add) %in% colnames(fma_curr)]

fma_new <- bind_rows(fma_curr, polys2add)
fma_new$OBJECTID_1 <- rownames(fma_new)

## inspect + confirm differences --------------------------------------------------------------
gg_old <- ggplot() +
  geom_sf(data = fma_curr, aes(fill = Name)) +
  guides(fill = "none")

gg_new <- ggplot() +
  geom_sf(data = fma_new, aes(fill = Name)) +
  guides(fill = "none")

cowplot::plot_grid(gg_old, gg_new)

## save to disk and archive -------------------------------------------------------------------
f_new <- file.path("inputs", "FMA_Boundaries", "FMA_Boundary_Updated_2024.shp")
st_write(fma_new, f_new)

zip_new <- paste0(tools::file_path_sans_ext(f_new), ".zip")
withr::with_dir(dirname(f_new), {
  archive::archive_write_files(
    archive = basename(zip_new),
    files = list.files(".", pattern = basename(tools::file_path_sans_ext(f_new))) |>
      grep("[.]zip$", x = _, invert = TRUE, value = TRUE)
  )
})

## upload the new zip file --------------------------------------------------------------------
gid <- drive_put(zip_new, as_id("1LsYuuYICkcpElAkEABFM5zJXf5tTyMLG"))

gid$id ## use this drive file id in the preamble module
