## Data download

```r
## manually download proprietary data
drive_download(as_id("https://drive.google.com/open?id=1M_L-7ovDpJLyY8dDOxG3xQTyzPx2HSg4"), "m/LandWebProprietaryData/data/SPP_1990_100m_NAD83_LCC_BYTE_VEG_NO_TIES_FILLED_FINAL.zip")

drive_download(as_id("https://drive.google.com/file/d/1y0ofr2H0c_IEMIpx19xf3_VTBheY0C9h"), "m/LandWebProprietaryData/data/CASFRI for Landweb.zip")
```

## Run the app

```r
## see app/App_Landweb.R
source("params/LandWeb_parameters.R")
file.copy("global_file.R", "global.R", overwrite = TRUE)
shiny::runApp(".", launch.browser = TRUE, port = 5921)
```
