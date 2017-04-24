library(raster)
library(SpaDES)
library(magrittr)
library(rgeos)
library(data.table)
PaulOnGoogleDrive <- raster("C:/data/LandWeb/LANDWEB_DATA_LAYERS/SPP_1990_FILLED_100m_NAD83_LCC_BYTE_VEG.dat")
#PaulOnGoogleDrive <- raster("C:/data/LandWeb/LANDWEB_DATA_LAYERS/")
#PaulOnGoogleDrive[] <- PaulOnGoogleDrive[]
PaulOnGoogleDriveTies <- raster(PaulOnGoogleDrive)
PaulOnGoogleDrive[] <- PaulOnGoogleDrive[]
PaulOnGoogleDriveTies[] <- PaulOnGoogleDriveTies[]
PaulOnGoogleDriveTies[] <- 0
PaulOnGoogleDriveTies[PaulOnGoogleDrive[]==201] <- 1
PaulOnGoogleDriveTies[PaulOnGoogleDrive[]==255] <- NA
#PaulOnGoogleDrive[PaulOnGoogleDrive>=200] <- NA
dev()
setPaths()
loadShpAndMakeValid <- function(file) {
shapefile(file) %>% gBuffer(byid=TRUE, width=0)
}
shpStudyRegionFull <- SpaDES::Cache(loadShpAndMakeValid, file=file.path("C:","Eliot","GitHub","LandWeb","inputs","shpLandWEB.shp"))
StudyAreaWeHave_IThinkCorrect <- sp::spTransform(shpStudyRegionFull, crs(PaulOnGoogleDrive))
CASFRI <- raster("C:/data/LandWeb/Landweb_CASFRI_GIDs.tif")
CASFRI <- setMinMax(CASFRI)
CASFRI[] <- CASFRI[]
CASFRI
CASFRIattr <- fread("C:/data/LandWeb/CASFRI for Landweb/Landweb_CASFRI_GIDs_attributes3.csv")
CASFRIheader <- fread("C:/data/LandWeb/CASFRI for Landweb/Landweb_CASFRI_GIDs_README.txt", skip = 14, nrows=25, sep = "\t", header = FALSE)
setnames(CASFRIattr, CASFRIheader$V1)
set(CASFRIattr, , grep(CASFRIheader$V1, pattern = "^SPECIES|^GID|^AGE", invert = TRUE), NULL)
setkey(CASFRIattr, "GID")
NAVals <- c("XXXX MISS", "UNDEF", "XXXX ERRC")
for(i in 1:5) {
set(CASFRIattr, which(CASFRIattr[[paste0("SPECIES_",i)]]%in%NAVals), paste0("SPECIES_",i), NA_character_ )
set(CASFRIattr, which(CASFRIattr[[paste0("SPECIES_PER_",i)]]%in%NAVals), paste0("SPECIES_",i), NA_character_ )
}
for(i in 1:1) {
CASFRIattr <- CASFRIattr[which(CASFRIattr[[paste0("SPECIES_PER_",i)]]>15),]
}
for(i in 2:5) {
set(CASFRIattr, which(CASFRIattr[[paste0("SPECIES_PER_",i)]]<=15), paste0("SPECIES_",i), NA_character_)
}
CASFRIattr[SPECIES_5>15,.N]
spAbund <- CASFRIattr[,.N,by="SPECIES_1"] %>% setkeyv("N") #%>% print()
spAbund2 <- CASFRIattr[,.N,by="SPECIES_2"] %>% setkeyv("N") #%>% print()
setorder(spAbund, -N)
setorder(spAbund2, N)
keepSpecies <- data.table(keepSpecies=spAbund$SPECIES_1[1:16])
set(keepSpecies, ,"spGroup", keepSpecies$keepSpecies)
setkey(keepSpecies, keepSpecies)
keepSpecies <- keepSpecies[!"Pseu menz"]
keepSpecies[c("Pice glau", "Pice enge", "Pice hybr", "Pice spp."),spGroup:="Pice_Gla"]
keepSpecies["Pice mari",spGroup:="Pice_Mar"]
keepSpecies["Betu papy",spGroup:="Betu_pap"]
keepSpecies[c("Abie bals", "Abie lasi"),spGroup:="Abie_sp"]
keepSpecies[c("Lari lari"),spGroup:="Lari_lar"]
keepSpecies[c("Pinu cont", "Pinu conl"),spGroup:="Pinu_con"]
keepSpecies[c("Pinu bank", "Pinu spp."),spGroup:="Pinu_ban"]
keepSpecies[c("Popu trem", "Popu balb"),spGroup:="Popu_tre"]

spRas <- list()
CA <- melt(CASFRIattr, id.vars = c("GID"),
     measure.vars = paste0("SPECIES_",1:5))
CA2 <- melt(CASFRIattr, id.vars = c("GID"),
           measure.vars = c(paste0("SPECIES_PER_",1:5)))
for(sp in unique(keepSpecies$spGroup)) {
  spRas[[sp]] <- raster(CASFRI)
  spRas[[sp]] <- 11
}



#levels(CASFRI) <- CASFRIattr
setindexv(CASFRIattr, "GID")

bet <- raster(CASFRI)
bet[] <- NA
aa <- CASFRI[] %in% CASFRIattr[SPECIES_1=="Betu papy",GID]
bet[aa] <- 1
