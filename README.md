## LandWeb v1.0.0

### Exploring Natural Range of Variation in the Western Boreal Forest

**Authors:**

- Eliot McIntire (eliot.mcintire@canada.ca)
- Alex M. Chubaty (achubaty@friresearch.ca)

**Date:** May 16, 2018

#### Overview

The LandWeb model integrates two well-used models for forest stand succession and fire simulation, implemented in the `SpaDES` simulation platform (Chubaty & McIntire, 2018).
Vegetation dynamics are modeled using an implementation of the LANDIS-II Biomass Succession model (Scheller & Mladenoff, 2004; Scheller *et al.*, 2007).
Fire dynamics are modeled using an implementation of LandMine (Andison, 1996).

Simulations were run for the entire LandWeb study area, which spans most of the western Canadian boreal forest.
Input data were derived from several publically available remote-sensed datasets (Beaudoin *et al.*, 2014), as well as proprietary data complied by Pickell *et al.*.

Simulation outputs consist of maps showing the time since fire as well as histogram summaries of 1) number of large patches (i.e., patches above the number of hectars specified by the user) contained within the selected spatial area; and 2) the vegetation cover within the selected spatial area.
Histograms are provided for each spatial area by polygon, age class, and species.
Authorized users can additionally overlay current stand conditions onto these histograms.
Simulation outputs are summarized for several publically available reporting polygons (including national ecozones, and ecodistricts) as well as several proprietary spatial areas (authorized users only).
Authorized users can additonally upload their own erporting polygons on which to summarize model results.

#### Using the model and app

Select pages from the menu on the left navigation menu.

- Login using your Google account to access additional features.
- **App Information:** additonal information about the app and model.
- **NRV:** view model results.
- **Model Details:** detailed information regording the model structure and its implementation in `SpaDES`.

#### Model inputs

**Public data sources:**

- Land Cover Classification 2005 map: ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip;
- LANDIS-II species traits: https://github.com/dcyr/LANDIS-II_IA_generalUseFiles;
- LANDIS-II parameterization tables and data: https://github.com/LANDIS-II-Foundation/Extensions-Succession-Archive/master/biomass-succession-archive/trunk/tests/v6.0-2.0/;
- Canada biomass, stand volume, and species data (from Beaudoin *et al., 2014): http://tree.pfc.forestry.ca;
- National ecodistrict polygons: http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip;
- National ecoregion polygons: http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip;
- National ecozone polygons: http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip.

**Proprietary data:** 

Stored in an access-controlled Google Drive location.

- biomass by species maps created by Pickell et al. (UBC) resolution 100m x 100m from LandSat and kNN based on CASFRI;
- reporting polygons used to summarize model results in the app.

#### Model outputs

Model outputs are available to download for authorized app users, and are described below.

##### Currently Selected Spatial Area

- Currently selected reporting polygon (`.shp`)

##### Large Patches Data for study region

- Large Patches Data (`.csv`)
- Large patches histograms (`.png`)

##### Leading Vegetation Cover Data for study region

- Leading Vegetation Cover Data (`.csv`)
- Leading Vegetation Cover histograms (`.png`)

##### Simulation Rasters (cropped to study region)

- Flammability maps (`.grd`)
- Time Since Fire maps (`.tif`)
- Vegetation type maps (`.grd`, `.tif`)

#### Additional R Data Files (advanced users)

- Simulation data files (`.RData`, `.rds`)

The downloaded outputs are bundled into a zip file (*e.g.*, `LandWeb_v1.0.0_2018-04-13.zip`) with the following directory and file structure:

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
 |_ README.md
 |_ simulationOutput_*.rds
 |_ vegArea.csv
```

#### References

Beaudoin, et al. (2014). Mapping attributes of Canada’s forests at moderate resolution through kNN and MODIS imagery. Canadian Journal of Forest Research, 44, 521–532. http://doi.org/10.1139/cjfr-2013-0401. Data available from http://tree.nfis.org/.

Chubaty, A. M. & McIntire, E. J. B. (2018) `SpaDES`: Develop and Run Spatially Explicit Discrete Event Simulation Models. R package version 2.0.1.
https://CRAN.R-project.org/package=SpaDES

Scheller, R. M., & Mladenoff, D. J. (2004). A forest growth and biomass module for a landscape simulation model, LANDIS: Design, validation, and application. Ecological Modelling, 180, 211–229. http://doi.org/10.1016/j.ecolmodel.2004.01.022

Scheller, R. M., Domingo, J. B., Sturtevant, B. R., Williams, J. S., Rudy, A., Gustafson, E. J., & Mladenoff, D. J. (2007). Design, development, and application of LANDIS-II, a spatial landscape simulation model with flexible temporal and spatial resolution. Ecological Modelling, 201, 409–419. http://doi.org/10.1016/j.ecolmodel.2006.10.009
