#!/bin/bash

until [ -f "mySimOut_1000.rds" ]
do
Rscript "newStart.R"
done

