#!/bin/bash

## USAGE: ./run_fma.sh <rep> <fma>
## provide a numeric <rep> as the first and only argument to this script

printf -v RUN "%02g" $1 ## assign to RUN, padding with extra zeros as needed
FMA=$2

OUTDIR="outputs/${FMA}_highDispersal_logROS"
RUNNAME="${FMA}_highDispersal_logROS_rep${RUN}"
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
