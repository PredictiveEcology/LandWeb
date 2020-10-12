#!/bin/bash

## USAGE: ./run_landweb.sh <rep>
## provide a numeric <rep> as the first and only argument to this script

printf -v RUN "%02g" $1 ## assign to RUN, padding with extra zeros as needed

OUTDIR="outputs/LandWeb_v3"
RUNNAME="LandWeb_v3_rep${RUN}"
RCMD="runName <- '${RUNNAME}'; source('00-global.R')"

if [ ! -d ${OUTDIR} ]; then
  mkdir -p ${OUTDIR}
fi

echo ${RCMD} | r

if [ -f "outputs/${RUNNAME}/rstTimeSinceFire_year1000.tif" ]; then
  mv "outputs/${RUNNAME}" "${OUTDIR}/rep${RUN}"
fi
