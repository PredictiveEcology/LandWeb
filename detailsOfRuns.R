ssh -L 8080:132.156.148.172:80 inf2.pfc.forestry.ca
ssh -L 22:132.156.148.172:22 inf2.pfc.forestry.ca


cF <- paths4sim$Free$cachePath
showCache(cF, userTags = "experiment")[tagKey=="cacheId"]$tagValue
cP <- paths4sim$Proprietary$cachePath
showCache(cP, userTags = "experiment")[tagKey=="cacheId"]$tagValue

cP <- paths$cachePath
dput(showCache(cP, userTags = "Map", after = Sys.time() - 60)[tagKey=="cacheId"]$tagValue)

a <- showCache(paths$cachePath)
data.table::setkey(a, "createdDate")
a[artifact == artifact[NROW(artifact)] & tagKey=="cacheId"]$tagValue


cP <- paths$cachePath
showCache(cP, userTags = "simInitAndExperiment")[tagKey=="cacheId"]$tagValue
cP <- paths$cachePath
showCache(cP, userTags = "gdal2Tiles")[tagKey=="cacheId"]$tagValue


cP <- paths$cachePath
dput(showCache(cP, userTags = c("reprojectRasts", "vtm"))[tagKey=="cacheId"]$tagValue)
cP <- paths$cachePath
dput(showCache(cP, userTags = c("reportingAndLeadingFn"))[tagKey=="cacheId"]$tagValue)


1. Get Free and Proprietary on one machine, copying cache/Free
2. 

# Overall model times # start is default at 0
endTime <- 1000
summaryInterval <- 10
summaryPeriod <- c(700, endTime)

# cacheId for 1000 years: 2e35699c4ade1b4bfa82e864558c7436, 7.3 days - on 342
authenticationType <- list("Free") # Can do one or both of "Free" "Proprietary"
# Spatial stuff -- determines the size of the area that will be "run" in the simulations
subStudyRegionName <- if (nzchar(Sys.getenv("subStudyRegionName"))) {
  Sys.getenv("subStudyRegionName")
} else {
  "SMALL"  #other options: "FULL", "EXTRALARGE", "LARGE", "MEDIUM", "NWT", "SMALL" , "RIA"
}                              #other options: "BC", "AB", "SK", "MB" or combinations, please specify in West-East order


# Nothing
screen

cd Documents/GitHub/LandWeb/
  R -e 'source("App_MB.R")'

# 171
screen

cd Documents/GitHub/LandWeb/
  R -e 'source("App_NWT.R")'

# Nothing
screen

cd Documents/GitHub/LandWeb/
R -e 'source("App_BC.R")'

# 172
screen

cd Documents/GitHub/LandWeb/
R -e 'source("App_SK.R")'

# 388
screen

cd Documents/GitHub/LandWeb/
R -e 'source("App_AB.R")'

screen

cd Documents/GitHub/LandWeb/
R -e 'source("App_LandWeb.R")'

############# FREE


# 171
screen

cd Documents/GitHub/LandWeb/
  subStudyRegionName="MB"
export subStudyRegionName
R -e 'source("global_file.R")'

# 172
screen

cd Documents/GitHub/LandWeb/
  subStudyRegionName="NWT"
export subStudyRegionName
R -e 'source("global_file.R")'

# 172
screen

cd Documents/GitHub/LandWeb/
  subStudyRegionName="BC"
export subStudyRegionName
R -e 'source("global_file.R")'

# 388
screen

cd Documents/GitHub/LandWeb/
  subStudyRegionName="AB"
export subStudyRegionName
R -e 'source("global_file.R")'

# 388
screen

cd Documents/GitHub/LandWeb/
  subStudyRegionName="SK"
export subStudyRegionName
R -e 'source("global_file.R")'



echo $subStudyRegionName


ls cache
rm -R cache/AB*
rm -R outputs/AB*


# Copy from any computer to 342/172
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/cache/NWT/* /home/emcintir/Documents/GitHub/LandWeb/cache/NWT/

# NWT Proprietary
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/cache/NWT_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/cache/NWT_Proprietary/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/cache/NWT/* /home/emcintir/Documents/GitHub/LandWeb/cache/NWT/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/outputs/NWT_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/outputs/NWT_Proprietary/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/NWT/* /home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/NWT/

# NWT Free
# Already on 172 

# SK
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/cache/SK/* /home/emcintir/Documents/GitHub/LandWeb/cache/SK/
# SK Proprietary
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/cache/SK_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/cache/SK_Proprietary/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/outputs/SK_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/outputs/SK_Proprietary/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/SK/* /home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/SK/

# SK Free
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/cache/SK_Free/* /home/emcintir/Documents/GitHub/LandWeb/cache/SK_Free/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/outputs/SK_Free/* /home/emcintir/Documents/GitHub/LandWeb/outputs/SK_Free/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/www/Free/SK/* /home/emcintir/Documents/GitHub/LandWeb/www/Free/SK/

# BC Proprietary
# Already on 172

# BC Free
# Already on 172

# MB
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/cache/MB/* /home/emcintir/Documents/GitHub/LandWeb/cache/MB/
# MB Proprietary
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/cache/MB_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/cache/MB_Proprietary/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Proprietary/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/MB/* /home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/MB/

# MB Free
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/cache/MB_Free/* /home/emcintir/Documents/GitHub/LandWeb/cache/MB_Free/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Free/* /home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Free/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/www/Free/MB/* /home/emcintir/Documents/GitHub/LandWeb/www/Free/MB/

# AB
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/cache/AB/* /home/emcintir/Documents/GitHub/LandWeb/cache/AB/
# AB Proprietary
# Already on 172

# AB Free
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/cache/AB_Free/* /home/emcintir/Documents/GitHub/LandWeb/cache/AB_Free/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/outputs/AB_Free/* /home/emcintir/Documents/GitHub/LandWeb/outputs/AB_Free/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/www/Free/AB/* /home/emcintir/Documents/GitHub/LandWeb/www/Free/AB/

# FULL
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/cache/FULL/* /home/emcintir/Documents/GitHub/LandWeb/cache/FULL/
# FULL Proprietary
# Already on 172

# FULL Free
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/cache/FULL_Free/* /home/emcintir/Documents/GitHub/LandWeb/cache/FULL_Free/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/outputs/FULL_Free/* /home/emcintir/Documents/GitHub/LandWeb/outputs/FULL_Free/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/www/Free/FULL/* /home/emcintir/Documents/GitHub/LandWeb/www/Free/FULL/


#############################################################################################################################
#############################################################################################################################
# Copy from 172 to .44
# FULL
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/FULL/* /home/emcintir/Documents/GitHub/LandWeb/cache/FULL/

# FULL Proprietary
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/FULL_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/cache/FULL_Proprietary/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/FULL_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/outputs/FULL_Proprietary/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/FULL/* /home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/FULL/

# FULL Free
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/FULL_Free/* /home/emcintir/Documents/GitHub/LandWeb/cache/FULL_Free/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/FULL_Free/* /home/emcintir/Documents/GitHub/LandWeb/outputs/FULL_Free/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Free/FULL/* /home/emcintir/Documents/GitHub/LandWeb/www/Free/FULL/

# MB
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/MB/* /home/emcintir/Documents/GitHub/LandWeb/cache/MB/

# MB Proprietary
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/MB_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/cache/MB_Proprietary/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Proprietary/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/MB/* /home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/MB/

# MB Free
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/MB_Free/* /home/emcintir/Documents/GitHub/LandWeb/cache/MB_Free/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Free/* /home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Free/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Free/MB/* /home/emcintir/Documents/GitHub/LandWeb/www/Free/MB/

# BC
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/BC/* /home/emcintir/Documents/GitHub/LandWeb/cache/BC/

# BC Proprietary
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/BC_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/cache/BC_Proprietary/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/BC_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/outputs/BC_Proprietary/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/BC/* /home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/BC/

# BC Free
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/BC_Free/* /home/emcintir/Documents/GitHub/LandWeb/cache/BC_Free/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/BC_Free/* /home/emcintir/Documents/GitHub/LandWeb/outputs/BC_Free/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Free/BC/* /home/emcintir/Documents/GitHub/LandWeb/www/Free/BC/

# AB
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/AB/* /home/emcintir/Documents/GitHub/LandWeb/cache/AB/

# AB Proprietary
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/AB_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/cache/AB_Proprietary/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/AB_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/outputs/AB_Proprietary/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/AB/* /home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/AB/

# AB Free
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/AB_Free/* /home/emcintir/Documents/GitHub/LandWeb/cache/AB_Free/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/AB_Free/* /home/emcintir/Documents/GitHub/LandWeb/outputs/AB_Free/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Free/AB/* /home/emcintir/Documents/GitHub/LandWeb/www/Free/AB/



#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
##### From 172 to Laptop
cd /c/Eliot/GitHub/LandWeb
cd /f/LandWeb

# NWT
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/NWT/* cache/NWT/
# NWT Proprietary

rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/NWT_Proprietary/* cache/NWT_Proprietary/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/NWT_Proprietary/* outputs/NWT_Proprietary/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/NWT/* www/Proprietary/NWT/

# NWT Free
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/NWT_Free/* cache/NWT_Free/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/NWT_Free/* outputs/NWT_Free/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Free/NWT/* www/Free/NWT/

# SK
#rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/SK/* cache/SK/
# SK Proprietary
#rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/SK_Proprietary/* cache/SK_Proprietary/
#rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/SK_Proprietary/* outputs/SK_Proprietary/
#rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/SK/* www/Proprietary/SK/

# SK Free
#rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/SK_Free/* cache/SK_Free/
#rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/SK_Free/* outputs/SK_Free/
#rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Free/SK/* www/Free/SK/

# BC
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/cache/BC/* cache/BC/
# BC Proprietary
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/cache/BC_Proprietary/* cache/BC_Proprietary/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/outputs/BC_Proprietary/* outputs/BC_Proprietary/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/BC/* www/Proprietary/BC/

# BC Free
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/cache/BC_Free/* cache/BC_Free/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/outputs/BC_Free/* outputs/BC_Free/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/www/Free/BC/* www/Free/BC/

# MB
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/cache/MB/* cache/MB/
# MB Proprietary
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/cache/MB_Proprietary/* cache/MB_Proprietary/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Proprietary/* outputs/MB_Proprietary/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/MB/* www/Proprietary/MB/

# MB Free
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/cache/MB_Free/* cache/MB_Free/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Free/* outputs/MB_Free/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/www/Free/MB/* www/Free/MB/

# AB
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/cache/AB/* cache/AB/
# AB Proprietary
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/cache/AB_Proprietary/* cache/AB_Proprietary/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/outputs/AB_Proprietary/* outputs/AB_Proprietary/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/AB/* www/Proprietary/AB/

# AB Free
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/cache/AB_Free/* cache/AB_Free/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/outputs/AB_Free/* outputs/AB_Free/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/www/Free/AB/* www/Free/AB/

# FULL
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/FULL/* cache/FULL/
# FULL Proprietary
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/FULL_Proprietary/* cache/FULL_Proprietary/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/FULL_Proprietary/* outputs/FULL_Proprietary/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/FULL/* www/Proprietary/FULL/

# FULL Free
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/FULL_Free/* cache/FULL_Free/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/FULL_Free/* outputs/FULL_Free/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Free/FULL/* www/Free/FULL/


###################
# From 172 -- to whatever Linux machine you are on
# NWT
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/NWT/* /home/emcintir/Documents/GitHub/LandWeb/cache/NWT/
# NWT Proprietary
#rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/NWT_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/cache/NWT_Proprietary/
#rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/NWT_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/outputs/NWT_Proprietary/
#rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/NWT/* /home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/NWT/

# NWT Free
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/NWT_Free/* /home/emcintir/Documents/GitHub/LandWeb/cache/NWT_Free/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/NWT_Free/* /home/emcintir/Documents/GitHub/LandWeb/outputs/NWT_Free/
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Free/NWT/* /home/emcintir/Documents/GitHub/LandWeb/www/Free/NWT/


################################# -- delete

# MB
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/MB/* cache/MB/ --delete
# MB Proprietary
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/MB_Proprietary/* cache/MB_Proprietary/ --delete
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Proprietary/* outputs/MB_Proprietary/ --delete
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/MB/* www/Proprietary/MB/ --delete

# MB Free
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/cache/MB_Free/* cache/MB_Free/ --delete
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Free/* outputs/MB_Free/ --delete
rsync -avzh emcintir@132.156.148.172:/home/emcintir/Documents/GitHub/LandWeb/www/Free/MB/* www/Free/MB/ --delete


#################
showCache(after = "2018-04-08 10:35:01")
showCache(paths4sim$Free$cachePath, after = "2018-04-08 10:35:01")
showCache(paths4sim$Proprietary$cachePath, after = "2018-04-08 10:35:01")

keepCache(after = "2018-04-08 10:35:01")
keepCache(paths4sim$Free$cachePath, after = "2018-04-08 10:35:01")
keepCache(paths4sim$Proprietary$cachePath, after = "2018-04-08 10:35:01")

