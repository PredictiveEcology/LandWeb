#!/bin/bash

## USAGE: ./run_fmu.sh <rep> <fma>
## provide a numeric <rep> as the first and only argument to this script

printf -v RUN "%02g" $1 ## assign to RUN, padding with extra zeros as needed
FMU=$2

OUTDIR="outputs/FMU_${FMU}_logROS"
RUNNAME="FMU_${FMU}_logROS_rep${RUN}"
RCMD="runName <- '${RUNNAME}'; source('newStart.R')"

if [ ! -d ${OUTDIR} ]; then
  mkdir -p ${OUTDIR}
fi

for i in {1..10}
do
  echo ${RCMD} | r
done

if [ -f "outputs/${RUNNAME}/rstTimeSinceFire_year1000.tif" ]; then
  mv "outputs/${RUNNAME}" "${OUTDIR}/rep${RUN}"
fi

