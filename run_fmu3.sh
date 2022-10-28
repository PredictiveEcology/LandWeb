#!/bin/bash

## USAGE: ./run_fmu.sh <fmu> <res> <rep>

FMU=$1
FRI=$2
RES=$3
printf -v RUN "%02g" $4 ## assign to RUN, padding with extra zeros as needed

RCMD=".mode <- 'production'; .studyAreaName <- 'FMU_${FMU}'; .res <- '${RES}'; .rep <- '${RUN}'; .version <- 3; source('00-global.R')"

echo ${RCMD} | r
