# LandWeb v1.0.0

**Authors:** Eliot McIntire and Alex M. Chubaty (Natural Resources Canada)

**Date:** April 16, 2018

## Overview

Lorem ipsum...

## Model outputs

Model outputs are available to download for authorived app users.
The downloaded file (*e.g.*, `LandWeb_v1.0.0_2018-04-13.zip`) can be unzipped and contains the following directory and file structure:

```
LandWeb_v1.0.0_2018-04-30/
 |_ histograms/
     |_ largePatches/
         |_ 500/                  ## currently selected patchSize
     |_ vegAgeMod/
 |_ polygons/
 |_ rasters/
     |_ rstTimeSinceFire_*.tif
     |_ vegTypeMap_*.tif*
 |_ experiment.RData
 |_ largePatches.csv
 |_ README.Rmd
 |_ simulationOutput_*.rds
 |_ vegArea.csv
```

### "Inputs"

- Currently selected reporting polygon (.shp)
- Cached raster input files (.tif)
- Inital communities map (.tif)

### Large Patches Data for study region

- Large Patches Data (.csv)
- Large patches histograms (.png)

### Leading Vegetation Cover Data for study region

- Leading Vegetation Cover Data (.csv)
- Leading Vegetaiton Cover histograms (.png)

### Simulation Rasters (cropped to study reagion)

- Flammability maps (.grd)
- Time Since Fire maps (.tif)
- Vegetation type maps (.grd, .tif)

## Additional R Data Files

- Simulation data files (.RData, .rds)
