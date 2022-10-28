#!/bin/bash

## USAGE: ./run_landweb.sh <rep>
## provide a numeric <rep> as the first and only argument to this script

RES=250
printf -v RUN "%02g" $1 ## assign to RUN, padding with extra zeros as needed

RCMD=".mode <- 'production'; .studyAreaName <- 'LandWeb'; .res <- '${RES}'; .rep <- '${RUN}'; .version <- 3; source('00-global.R')"

echo ${RCMD} | r
