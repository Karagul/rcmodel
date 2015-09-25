# main.R
# 7/6/2015
# package deals

library(devtools)
library(assertthat)
library(testthat)
library(dplyr)
library(mgcv)
library(conditionalSample)

devtools::load_all()


# package documentation

# setup package utilities
use_package("assertthat")
use_package("testthat")
use_package("dplyr", type = "Depends")
use_package("mgcv")
use_package("conditionalSample")
use_package("nlme")
# use_package("markstats", type = "Depends")
use_package("R.utils")
use_testthat()

# document
devtools::document()


# test
devtools::test()


# set up to use data
# dir.create("data")
use_data(Phosphorus, overwrite = TRUE)
use_data(rc_synth, overwrite = TRUE)


# load and install package

devtools::install_github("markwh/markstats")
devtools::install()


