#!/bin/bash

## USAGE: ./run_fma.sh <rep> <fma>
## provide a numeric <rep> as the first and only argument to this script

RUN=$1
FMA=$2
RCMD1="runName <- paste0('${FMA}_highDispersal_logROS_rep', SpaDES.core::paddedFloatToChar(${RUN}, padL = 2));"
RCMD2="source('newStart.R')"

RCMDS="${RCMD1} ${RCMD2}"

for i in {1..10}
do
    echo ${RCMDS} | r
done

