# main.R
# 7/6/2015
# package deals

library(devtools)

library(assertthat)
library(testthat)
library(dplyr)
library(mgcv)
library(conditionalSample)

# package documentation

use_package("assertthat")
# use_package("testthat")
use_package("dplyr")
use_package("mgcv")
use_package("conditionalSample")
devtools::document()


# set up to use testthat for tests:

# set up to use data

dir.create("data")
?devtools::use_data

load("../../Research/ExtremeEvents/cache/samplePData.Rdata")
use_data(sampleData, overwrite = TRUE)


# load and install package

devtools::load_all()
devtools::install()
