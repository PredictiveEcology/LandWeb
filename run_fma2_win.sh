#!/bin/bash

## USAGE: ./run_fma.sh <fma> <rep>

FMA=$1
RES=250
printf -v RUN "%02g" $2 ## assign to RUN, padding with extra zeros as needed

RCMD=".mode <- 'production'; .studyAreaName <- '${FMA}'; .res <- '${RES}'; .rep <- '${RUN}'; .version <- 2; source('00-global.R')"

echo ${RCMD} | r
