#!/bin/bash

## USAGE: ./run_landweb.sh <rep>
## provide a numeric <rep> as the first and only argument to this script

RES=250
printf -v RUN "%02d" $1 ## assign to RUN, padding with extra zeros as needed
VERS=3

RCMD=".mode <- 'production'; .studyAreaName <- 'LandWeb'; .res <- ${RES}; .rep <- ${RUN}; .version <- ${VERS}; source('00-global.R')"

echo ${RCMD} | xvfb-run -a r
