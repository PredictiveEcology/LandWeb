### LandWeb was previosuly only using Boreal Caribou Ranges for results
### This adds Mountain Caribou Ranges for AB and BC, as requested by several partners

library(magrittr)
library(map)
library(stringr)

ml <- mapAdd(layerName = "Boreal Caribou Ranges",
             useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
             url = "https://drive.google.com/file/d/1PYLou8J1wcrme7Z2tx1wtA4GvaWnU1Jy/view?usp=sharing",
             columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)
prj <- proj4string(ml[["Boreal Caribou Ranges"]])

## Info: https://geodiscover.alberta.ca/geoportal/catalog/search/resource/details.page?uuid=%7BA4588C9B-3310-46D6-A37C-F2C6AB1B86A2%7D
## Data: https://extranet.gov.ab.ca/srd/geodiscover/srd_pub/LAT/FWDSensitivity/CaribouRange.zip
## See Also: https://open.alberta.ca/dataset/932d6c22-a32a-4b4e-a3f5-cb2703c53280/resource/8335d979-394e-4959-ac5c-014dc2106df9/download/albertacaribouranges-map-nov2017.pdf
ab.caribou <- shapefile("~/GitHub/LandWeb/inputs/CaribouRange/Caribou_Range.shp") %>%
  spTransform(., prj)
## TODO: need to extract the non-boreal ranges and add them to the caribou range map
boreal.sub.ab <- ml[["Boreal Caribou Ranges"]][["Name"]] %>%
  str_remove(., ".\\([A-Z]{3}\\)") %>%
  str_remove(., ".River")
omit.ab <- pmatch(boreal.sub.ab, ab.caribou[["SUBUNIT"]]) %>% na.omit() %>%
  c(., which(ab.caribou[["STATUS"]] != "Active")) %>%
  c(., which(ab.caribou[["LOCALRANGE"]] == "East Side Athabasca")) %>%
  c(., which(ab.caribou[["SUBUNIT"]] == "Bischto"))
ab.caribou[-omit.ab,]$SUBUNIT

## Info: https://catalogue.data.gov.bc.ca/dataset/caribou-herd-locations-for-bc
bc.caribou <- shapefile("~/GitHub/LandWeb/inputs/BCGW_7113060B_1561059191195_8572/GCPB_CARIBOU_POPULATION_SP/GCBP_CARIB_polygon.shp") %>%
  spTransform(., prj)
## TODO: need to extract the non-boreal ranges and add them to the caribou range map
boreal.sub.bc <- ml[["Boreal Caribou Ranges"]][["Name"]] %>% str_remove(., ".\\([A-Z]{3}\\)")
omit.bc <- pmatch(boreal.sub.bc, bc.caribou[["HERD_NAME"]]) %>% na.omit() %>%
  c(., which(bc.caribou[["ECOTYPE"]] == "Boreal")) %>%
  c(., which(bc.caribou[["HERD_STAT"]] != "Herd"))
bc.caribou[-omit,][["HERD_NAME"]]

## Plotting
plot(ml[["Boreal Caribou Ranges"]], col = "pink")
plot(ab.caribou[-omit.ab,], add = TRUE, col = "blue")
plot(bc.caribou[-omit.bc,], add = TRUE, col = "orange")

### merge these 3 layers, keeping herd names and matching shinyLabel
# drop all columns except 'SUBUNIT' in ab.caribou; rename to 'Name'
ab.caribou.new <- ab.caribou[-omit.ab, which(names(ab.caribou) == "SUBUNIT")]
names(ab.caribou.new) <- "Name"

# TODO: drop all columns except 'HERD_NAME' in bc.caribou; rename to 'Name'
bc.caribou.new <- bc.caribou[-omit.bc, which(names(bc.caribou) == "HERD_NAME")]
names(bc.caribou.new) <- "Name"

ab.bc.caribou <- rbind(ab.caribou.new, bc.caribou.new, makeUniqueIDs = TRUE)
lw.caribou.new <- ml[["Boreal Caribou Ranges"]][, which(names(ml[["Boreal Caribou Ranges"]]) == "Name")]
lw.caribou <- rbind(ab.bc.caribou, lw.caribou.new, makeUniqueIDs = TRUE)
lw.caribou$shinyLabel <- lw.caribou[["Name"]]

plot(lw.caribou, col = "lightblue")

dd <- "~/GitHub/LandWeb/inputs/Caribou_Ranges_LandWeb"
if (!dir.exists(dd)) dir.create(dd)
shapefile(lw.caribou, file = file.path(dd, "caribou_landweb.shp"))
