cF <- paths4sim$Free$cachePath
showCache(cF, userTags = "experiment")[tagKey=="cacheId"]$tagValue
cP <- paths4sim$Proprietary$cachePath
showCache(cP, userTags = "experiment")[tagKey=="cacheId"]$tagValue

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


# 171
screen

cd Documents/GitHub/LandWeb/
R -e 'source("App_MB.R")'

# 172
screen

cd Documents/GitHub/LandWeb/
R -e 'source("App_NWT.R")'


# 172
screen

cd Documents/GitHub/LandWeb/
R -e 'source("App_BC.R")'

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
# NWT Proprietary
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/cache/NWT_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/cache/NWT_Proprietary/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/outputs/NWT_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/outputs/NWT_Proprietary/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/NWT/* /home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/NWT/

# NWT Free
# Already on 172 

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

# MB Proprietary
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/cache/MB_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/cache/MB_Proprietary/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Proprietary/* /home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Proprietary/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/MB/* /home/emcintir/Documents/GitHub/LandWeb/www/Proprietary/MB/

# MB Free
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/cache/MB_Free/* /home/emcintir/Documents/GitHub/LandWeb/cache/MB_Free/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Free/* /home/emcintir/Documents/GitHub/LandWeb/outputs/MB_Free/
rsync -avzh emcintir@132.156.148.171:/home/emcintir/Documents/GitHub/LandWeb/www/Free/MB/* /home/emcintir/Documents/GitHub/LandWeb/www/Free/MB/

# AB Proprietary
# Already on 172

# AB Free
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/cache/AB_Free/* /home/emcintir/Documents/GitHub/LandWeb/cache/AB_Free/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/outputs/AB_Free/* /home/emcintir/Documents/GitHub/LandWeb/outputs/AB_Free/
rsync -avzh emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/www/Free/AB/* /home/emcintir/Documents/GitHub/LandWeb/www/Free/AB/


cd /c/Eliot/GitHub/LandWeb
rsync -n emcintir@132.156.149.44:/home/emcintir/Documents/GitHub/LandWeb/* .
