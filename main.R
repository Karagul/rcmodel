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
use_package("dplyr")
use_package("mgcv")
use_package("nlme")
# use_package("markstats", type = "Depends")
use_package("R.utils")
use_package("leaflet", type = "suggests")
use_package("udunits2", type = "suggests")
use_testthat()

# document
devtools::document()


# test
devtools::test()


# Vignettes
devtools::use_vignette("retrieval") # data retrieval


# Datasets ----------------------------------------------------------------
# dir.create("data")
use_data(Phosphorus, overwrite = TRUE)
use_data(rc_synth, overwrite = TRUE)

# internal, for functions
# unitTable <- read.csv("inst/unitTable.csv", stringsAsFactors = FALSE)
use_data(unitTable, internal = TRUE, overwrite = TRUE)

# load and install package ------------------------------------------------
devtools::install()


