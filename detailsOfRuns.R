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