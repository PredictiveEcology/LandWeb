#!/bin/bash

## USAGE: ./run_fma.sh <fma> <rep>

FMA=$1
RES=250
printf -v RUN "%02d" $2 ## assign to RUN, padding with extra zeros as needed
VERS=3

RCMD=".mode <- 'production'; .studyAreaName <- '${FMA}'; .res <- '${RES}'; .rep <- '${RUN}'; .version <- ${VERS}; source('00-global.R')"

echo ${RCMD} | r
