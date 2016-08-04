rm(list=ls())
library(testthat)
library(data.table)
library(raster)
library(SpaDES)
# top down tests contains the tests from overall simulation at higher level of functions
test_dir("~//GitHub//nrv-succession//code blitz succession//modules//biomassSuccessionLANDIS//tests//top_down tests")
# bottom up tests contains the tests of each subfunction within events
test_dir("~//GitHub//nrv-succession//code blitz succession//modules//biomassSuccessionLANDIS//tests//bottom_up tests")