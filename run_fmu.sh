#!/bin/bash

## USAGE: ./run_fmu.sh <fmu> <res> <rep>

FMU=$1
FRI=$2
RES=$3
printf -v RUN "%02g" $4 ## assign to RUN, padding with extra zeros as needed

OUTDIR1="outputs/FMU_${FMU}_fri${FRI}_res${RES}"
OUTDIR2="outputs/FMU_${FMU}/fri${FRI}/res${RES}"
OUTDIR3="${OUTDIR2}/rep${RUN}"
RUNNAME="FMU_${FMU}_fri${FRI}_res${RES}_rep${RUN}"
RCMD="runName <- '${RUNNAME}'; source('newStart.R')"

if [ ! -d ${OUTDIR2} ]; then
  mkdir -p ${OUTDIR2}
fi

echo ${RCMD} | r

if [ -f "outputs/${RUNNAME}/rstTimeSinceFire_year1000.tif" ]; then
  mv "outputs/${RUNNAME}" "${OUTDIR3}"
fi
