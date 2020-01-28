#!/bin/bash

## USAGE: ./run_fma.sh <fma> <rep>

FMA=$1
RES=250
printf -v RUN "%02g" $2 ## assign to RUN, padding with extra zeros as needed

OUTDIR="outputs/${FMA}_res${RES}"
RUNNAME="${FMA}_highDispersal_logROS_res${RES}_rep${RUN}"
RCMD="runName <- '${RUNNAME}'; .libPaths(''); source('newStart.R')"

if [ ! -d ${OUTDIR}/res${RES} ]; then
  mkdir -p ${OUTDIR}/res${RES}
fi

for i in {1..10}
do
  echo ${RCMD} | r
done

if [ -f "outputs/${RUNNAME}/rstTimeSinceFire_year1000.tif" ]; then
  mv "outputs/${RUNNAME}" "${OUTDIR}/res${RES}/rep${RUN}"
fi
