#!/bin/bash

## USAGE: ./run_fma.sh <fma> <rep>

FMA=$1
RES=250
printf -v RUN "%02g" $2 ## assign to RUN, padding with extra zeros as needed

OUTDIR="outputs/${FMA}_v3"
RUNNAME="${FMA}_v3_res${RES}_rep${RUN}"
RCMD="runName <- '${RUNNAME}'; source('00-global.R')"

if [ ! -d ${OUTDIR}/res${RES} ]; then
  mkdir -p ${OUTDIR}/res${RES}
fi

echo ${RCMD} | r
