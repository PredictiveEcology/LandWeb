## LandWeb v2.1.0

### Exploring Natural Range of Variation in the Western Boreal Forest

**Authors:**

- Eliot McIntire (<eliot.mcintire@nrcan-rncan.gc.ca>)
- Alex M. Chubaty (<achubaty@for-cast.ca>) [maintainer]

**Date:** August 15, 2023

**User Guide and Manual:** <https://predictiveecology.github.io/LandWeb/>

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

Full model (and module) descriptions and other information are provided in the [LandWeb user guide and manual](https://predictiveecology.github.io/LandWeb/).

#### Getting the code

```bash
git clone --recurse-submodules
  -j8 https://github.com/PredictiveEcology/LandWeb
```

See <https://predictiveecology.github.io/LandWeb/getting-started.html> for detailed instructions and prerequisite installation.

#### Running the model

See <https://predictiveecology.github.io/LandWeb/running-landweb.html> for detailed instructions on running LandWeb.

#### References

Beaudoin, et al. (2014). Mapping attributes of Canada’s forests at moderate resolution through kNN and MODIS imagery. Canadian Journal of Forest Research, 44, 521–532. http://doi.org/10.1139/cjfr-2013-0401. Data available from http://tree.nfis.org/.

Chubaty, A. M. & McIntire, E. J. B. (2019) `SpaDES`: Develop and Run Spatially Explicit Discrete Event Simulation Models. R package version 2.0.4. <https://CRAN.R-project.org/package=SpaDES>

Pickell, P. D., & Coops, N. C. (2016) Development of historical forest attribute layers using Landsat time series and kNN imputation for the western Canadian boreal forest. Technical Report, 27 pp.

Scheller, R. M., & Mladenoff, D. J. (2004). A forest growth and biomass module for a landscape simulation model, LANDIS: Design, validation, and application. Ecological Modelling, 180, 211–229. <https://doi.org/10.1016/j.ecolmodel.2004.01.022>

Scheller, R. M., Domingo, J. B., Sturtevant, B. R., Williams, J. S., Rudy, A., Gustafson, E. J., & Mladenoff, D. J. (2007). Design, development, and application of LANDIS-II, a spatial landscape simulation model with flexible temporal and spatial resolution. Ecological Modelling, 201, 409–419. <https://doi.org/10.1016/j.ecolmodel.2006.10.009>
