library(dplyr)
library(sf)

lthfc <- st_read("inputs/landweb_ltfc_v8.shp")
ecodistricts <- st_read("inputs/ecodistricts.shp") |>
  st_transform(st_crs(lthfc))

polys2add <- ecodistricts[ ecodistricts$ECODISTRIC %in% c(183, 270, 271, 272), ]
polys2add <- polys2add[, colnames(polys2add) %in% colnames(lthfc)]
polys2add$LTHFC <- 80L

lthfc_new <- bind_rows(lthfc, polys2add)

plot(lthfc_new["LTHFC"])

st_write(lthfc_new, "inputs/landweb_ltfc_v8a.shp")

withr::with_dir("inputs", {
  archive::archive_write_files(
    archive = "landweb_ltfc_v8a.zip",
    files = grep("[.]zip$", list.files(".", pattern = "landweb_ltfc_v8a"), invert = TRUE, value = TRUE)
  )
})

## upload the new zip file
library(googledrive)
gid <- drive_put("inputs/landweb_ltfc_v8a.zip", as_id("1LsYuuYICkcpElAkEABFM5zJXf5tTyMLG"))

gid$id ## use this drive file id in the preamble module
