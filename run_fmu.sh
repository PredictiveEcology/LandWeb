#!/bin/bash

## USAGE: ./run_fmu.sh <fmu> <res> <rep>

FMU=$1
RES=$2
printf -v RUN "%02g" $3 ## assign to RUN, padding with extra zeros as needed

OUTDIR="outputs/FMU_${FMU}_res${RES}"
RUNNAME="FMU_${FMU}_res${RES}_rep${RUN}"
RCMD="runName <- '${RUNNAME}'; source('newStart.R')"

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
