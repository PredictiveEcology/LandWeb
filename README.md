## LandWeb v2.1.0

### Exploring Natural Range of Variation in the Western Boreal Forest

**Authors:**

- Eliot McIntire (eliot.mcintire@canada.ca)
- Alex M. Chubaty (achubaty@for-cast.ca)

**Date:** July 27, 2021

### Getting the code

All modules are written in R and all model code was developed collaboratively using GitHub (<https://github.com>), with each module contained in its own git repository (see below).
Code that is shared among modules was bundled into R packages, and hosted in open git repositories.
All package code is automatically and regularly tested using cross-platform continuous integration frameworks to ensure the code is reliable and free of errors.

```bash
## master branch
git clone --recurse-submodules -j8 https://github.com/PredictiveEcology/LandWeb

## development branch
git clone --single-branch -b development \
  --recurse-submodules="." \
  --recurse-submodules=":(exclude)app" \
  --recurse-submodules=":(exclude)deploy" \
  -j8 https://github.com/PredictiveEcology/LandWeb
```

**NOTE:** you will need access to each of the following repositories:

_Modules_

- [PredictiveEcology/Biomass_borealDataPrep](https://github.com/PredictiveEcology/Biomass_borealDataPrep)
- [PredictiveEcology/Biomass_core](https://github.com/PredictiveEcology/Biomass_core)
- [PredictiveEcology/Biomass_speciesData](https://github.com/PredictiveEcology/Biomass_speciesData)
- [PredictiveEcology/Biomass_regeneration](https://github.com/PredictiveEcology/Biomass_regeneration)
- [PredictiveEcology/LandMine](https://github.com/PredictiveEcology/LandMine)
- [fRI-Research/LandWeb_app](https://github.com/fRI-Research/LandWeb_app)
- [fRI-Research/LandWeb_deploy](https://github.com/fRI-Research/LandWeb_deploy)
- [fRI-Research/LandWeb_output](https://github.com/fRI-Research/LandWeb_output)
- [fRI-Research/LandWeb_preamble](https://github.com/fRI-Research/LandWeb_preamble)
- [fRI-Research/timeSinceFire](https://github.com/fRI-Research/timeSinceFire)

_Packages_

- [PredictiveEcology/LandR](https://github.com/PredictiveEcology/LandR)
- [PredictiveEcology/LandWebUtils](https://github.com/PredictiveEcology/LandWebUtils)
- [PredictiveEcology/map](https://github.com/PredictiveEcology/map)
- [PredictiveEcology/pemisc](https://github.com/PredictiveEcology/pemisc)

### Prerequisites

First, verify your installation of package development tools by running:

```{r has_devel}
install.packages('devtools')
devtools::has_devel()
```

The code is mostly self-sufficient: additional packages than those below are needed, but will be installed automatically.
See `03-packages.R` to see which additional packages will be used.

#### Overview

The LandWeb model integrates two well-used models for forest stand succession and fire simulation, implemented in the `SpaDES` simulation platform (Chubaty & McIntire, 2019).
Vegetation dynamics are modeled using an implementation of the LANDIS-II Biomass Succession model (Scheller & Mladenoff, 2004; Scheller *et al.*, 2007).
Fire dynamics are modeled using an implementation of LandMine (Andison, 1996).

Simulations were run for the entire LandWeb study area, which spans most of the western Canadian boreal forest.
Input data were derived from several publically available remote-sensed datasets (Beaudoin *et al.*, 2014), as well as proprietary data complied by Pickell *et al.* (2016).

Simulation outputs consist of maps showing the time since fire as well as histogram summaries of 1) number of large patches (i.e., patches above the number of hectares specified by the user) contained within the selected spatial area; and 2) the vegetation cover within the selected spatial area.
Histograms are provided for each spatial area by polygon, age class, and species.
Authorized users can additionally overlay current stand conditions onto these histograms.
Simulation outputs are summarized for several forestry management areas/units.

#### Using the model and app

Select pages from the menu on the left navigation menu.

- Login using your Google account to access additional features.
- **App Information:** additonal information about the app and model.
- **NRV:** view model results.
- **Model Details:** detailed information regording the model structure and its implementation in `SpaDES`.

#### Model inputs

**Public data sources:**

- Land Cover Classification 2005 map: <ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip>;
- LANDIS-II species traits: <https://github.com/dcyr/LANDIS-II_IA_generalUseFiles>;
- LANDIS-II parameterization tables and data: <https://github.com/LANDIS-II-Foundation/Extensions-Succession-Archive/master/biomass-succession-archive/trunk/tests/v6.0-2.0/>;
- Canada biomass, stand volume, and species data (from Beaudoin *et al.*, 2014): <http://tree.pfc.forestry.ca>;
- National ecodistrict polygons: <http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip>;
- National ecoregion polygons: <http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip>;
- National ecozone polygons: <http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip>.

**Proprietary data:** 

Stored in an access-controlled Google Drive location.

- biomass by species maps created by Pickell et al. (2016) resolution 100m x 100m from LandSat and kNN based on CASFRI;
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
- Leading Vegetation Cover boxplots (`.png`)

##### Simulation Rasters (cropped to study region)

- Flammability maps (`.grd`)
- Time Since Fire maps (`.tif`)
- Vegetation type maps (`.grd`, `.tif`)

#### Additional R Data Files (advanced users)

- Simulation data files (`.RData`, `.rds`)

The downloaded outputs are bundled into a zip file (*e.g.*, `LandWeb_v2.0.0_2019-09-23.zip`) with the following directory and file structure:

```
LandWeb_v2.0.0_2019-09-23/
 |_ boxplots/
     |_ leading_*.csv
 |_ histograms/
     |_ largePatches/
         |_ 100/
         |_ 500/
         |_ 1000/
         |_ 5000/
     |_ leading/
     |_ largePatches_*.csv
 |_ polygons/
 |_ rasters/
     |_ rstTimeSinceFire_*.tif
     |_ vegTypeMap_*.tif*
 |_ README.md
 |_ simulationOutput_*.rds
```

#### References

Beaudoin, et al. (2014). Mapping attributes of Canada’s forests at moderate resolution through kNN and MODIS imagery. Canadian Journal of Forest Research, 44, 521–532. http://doi.org/10.1139/cjfr-2013-0401. Data available from http://tree.nfis.org/.

Chubaty, A. M. & McIntire, E. J. B. (2019) `SpaDES`: Develop and Run Spatially Explicit Discrete Event Simulation Models. R package version 2.0.4. <https://CRAN.R-project.org/package=SpaDES>

Pickell, P. D., & Coops, N. C. (2016) Development of historical forest attribute layers using Landsat time series and kNN imputation for the western Canadian boreal forest. Technical Report, 27 pp.

Scheller, R. M., & Mladenoff, D. J. (2004). A forest growth and biomass module for a landscape simulation model, LANDIS: Design, validation, and application. Ecological Modelling, 180, 211–229. <https://doi.org/10.1016/j.ecolmodel.2004.01.022>

Scheller, R. M., Domingo, J. B., Sturtevant, B. R., Williams, J. S., Rudy, A., Gustafson, E. J., & Mladenoff, D. J. (2007). Design, development, and application of LANDIS-II, a spatial landscape simulation model with flexible temporal and spatial resolution. Ecological Modelling, 201, 409–419. <https://doi.org/10.1016/j.ecolmodel.2006.10.009>
