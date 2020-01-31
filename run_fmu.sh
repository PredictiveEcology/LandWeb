#!/bin/bash

## USAGE: ./run_fmu.sh <fmu> <res> <rep>

FMU=$1
FRI=$2
RES=$3
printf -v RUN "%02g" $4 ## assign to RUN, padding with extra zeros as needed

OUTDIR="outputs/FMU_${FMU}_fri${FRI}_res${RES}"
RUNNAME="FMU_${FMU}_fri${FRI}_res${RES}_rep${RUN}"
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
