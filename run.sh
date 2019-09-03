#!/bin/bash

## USAGE: ./run.sh <rep>
## provide a numeric <rep> as the first and only argument to this script

printf -v RUN "%02g" $1 ## assign to RUN, padding with extra zeros as needed

OUTDIR="outputs/LandWeb_highDispersal_logROS"
RUNNAME="LandWeb_highDispersal_logROS_rep${RUN}"
RCMD="runName <- '${RUNNAME}'; source('newStart.R')"

if [ ! -d ${OUTDIR} ]; then
  mkdir -p ${OUTDIR}
fi

for i in {1..10}
do
  echo ${RCMD} | r
done

if [ -f "outputs/${RUNNAME}/mySimOut_1000.rds" ]; then
  mv "outputs/${RUNNAME}" "${OUTDIR}/rep${RUN}"
fi
