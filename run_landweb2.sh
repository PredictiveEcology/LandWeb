#!/bin/bash

## USAGE: ./run_landweb.sh <rep>
## provide a numeric <rep> as the first and only argument to this script

printf -v RUN "%02g" $1 ## assign to RUN, padding with extra zeros as needed

OUTDIR="outputs/LandWeb_highDispersal_logROS"
RUNNAME="LandWeb_highDispersal_logROS_rep${RUN}"
RCMD="runName <- '${RUNNAME}'; source('00-global.R')"

if [ ! -d ${OUTDIR} ]; then
  mkdir -p ${OUTDIR}
fi

echo ${RCMD} | r
