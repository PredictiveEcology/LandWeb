# Load Study Area
ml <- appendMapList(layerName = "LandWeb Study Area",
              sourceURL = "https://drive.google.com/open?id=1JptU0R7qsHOEAEkxybx5MGg650KC98c6",
              columnNameForLabels = "Name", studyArea = TRUE
)


# Load others
ml <- appendMapList(mapList = ml, sourceURL = "https://drive.google.com/file/d/1Oz2vSor3oIKf2uGv3KRtLoLRWEfX5Mas/view?usp=sharing",
              layerName = "Mountain Northern Caribou Ranges",
              columnNameForLabels = "Name")

ml <- appendMapList(mapList = ml, layerName = "Provincial Parks",
  sourceURL = "https://drive.google.com/file/d/1GHgTI4JY-YhAXvWkgV20vugbvLNqEEGH/view?usp=sharing",
       columnNameForLabels = "Name")
ml <- appendMapList(mapList = ml, layerName = "NWT Ecoregions",
  sourceURL = "https://drive.google.com/file/d/1iRAQfARkmS6-XVHFnTkB-iltzMNPAczC/view?usp=sharing",
  columnNameForLabels = "Name")
ml <- appendMapList(mapList = ml, layerName = "National Parks",
  sourceURL = "https://drive.google.com/file/d/1B3VUU8PDn4NPveAyF76OBPY0vZkxScEt/view?usp=sharing",
  columnNameForLabels = "Name")
ml <- appendMapList(mapList = ml, layerName = "AB Natural Sub Regions",
  sourceURL = "https://drive.google.com/file/d/1mCEynahKnFkStJUJC8ho5ndRD41olz9F/view?usp=sharing",
  columnNameForLabels = "Name")
# "LP MASTERFILE June62012",
#   sourceURL = "https://drive.google.com/file/d/1J38DKQQavjBV9F3z2gGzHNuNE0s2rmhh/view?usp=sharing",
#   columnNameForLabels = "Name"),
ml <- appendMapList(mapList = ml, layerName = "BC Bio Geoclimatic Zones",
  sourceURL = "https://drive.google.com/file/d/1VAwsax63l2akOM2j_O4Je9p0ZiYg8Hl-/view?usp=sharing",
  columnNameForLabels = "ZONE_NAME")
ml <- appendMapList(mapList = ml, layerName = "FMU Alberta 2015-11",
  sourceURL = "https://drive.google.com/file/d/1JiCLcHh5fsBAy8yAx8NgtK7fxaZ4Tetl/view?usp=sharing",
  columnNameForLabels = "FMU_NAME")
ml <- appendMapList(mapList = ml, layerName = "FMA Boundary Updated",
  sourceURL = "https://drive.google.com/file/d/1nTFOcrdMf1hIsxd_yNCSTr8RrYNHHwuc/view?usp=sharing",
  columnNameForLabels = "Name")
ml <- appendMapList(mapList = ml, layerName = "Boreal Caribou Ranges",
  sourceURL = "https://drive.google.com/file/d/1PYLou8J1wcrme7Z2tx1wtA4GvaWnU1Jy/view?usp=sharing",
  columnNameForLabels = "Name")


### RASTERS
# STOPPED HERE
# Current Condition
preProcess(url = "https://drive.google.com/file/d/1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1/view?usp=sharing")

ml <- appendMapList(mapList = ml, sourceURL = "https://drive.google.com/file/d/1Oz2vSor3oIKf2uGv3KRtLoLRWEfX5Mas/view?usp=sharing",
                    layerName = "Mountain Northern Caribou Ranges",
                    columnNameForLabels = "Name")
