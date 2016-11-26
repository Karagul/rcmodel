# loadest.R
# Mark Hagemann
# 3/31/2016
# Functions to compute loads in a fashion similar to LOADEST


#' LOADEST formulae
#'
loadest.form <- function(number) {
  if (number == 0)
    number = 1:9

  forms <- c(
    "c ~ q",           # 1.
    "c ~ poly(q, 2)",  # 2.
    "c ~ q + time",    # 3.
    "c ~ q + sharm(Date, 1)", # 4.
    "c ~ poly(q, 2) + time",  # 5.
    "c ~ poly(q, 2) + sharm(Date, 1)", # 6.
    "c ~ q + sharm(Date, 1) + time",   # 7.
    "c ~ poly(q, 2) + sharm(Date, 1) + time", # 8.
    "c ~ poly(q, 2) + sharm(Date) + poly(time, 2)" # 9.
  )
  forms[number]
}

#' Calibrate a model or series of models using LOADEST built-in forms
#'
#' Returns list with elements corresponding to typical LOADEST output files
#'
#' @param data An rcData object
#' @param modno The LOADEST model number, defaults to 0 (automatic selection)
#' @param fun.select function to use to select between models
#' @param which.select Either min or max, tells which value of select funciton is best
#' @importFrom assertthat assert_that
#' @export
loadest_cal <- function(data, modno = 0, fun.select = AIC, which.select = min) {

  # calibrate
  # validate, select
  # predict

  assert_that(mode(fun.select) == "function",
              mode(which.select) == "function")

  nos <- if (modno == 0)
    1:9 else
      modno

  forms <- loadest.form(modno)
  mods <- lapply(forms, as.formula) %>%
    lapply(rcgam, data = data)

  selvals <- vapply(mods, fun.select, numeric(1))
  bestno <- which(selvals == which.select(selvals))

  nos[bestno]
  forms[bestno]
  bestmod <- mods[[bestno]]

  bestmod
}

loadest_check <- function(object) {

}


#' Make predictions a la LOADEST
#' @param object a rcgam object
#' @param preddata either a rcData object or a data.frame that can be converted to one via makePredData
#' @param what either "load" for load (kg/day) or "conc" for concentration (mg/L)
#' @export
loadest_pred <- function(object, preddata, flow = NULL,  what = c("load", "conc"),
                         ...) {

  stopifnot(is(object, "rcgam"))
  stopifnot(is(preddata, "data.frame"))
  what <- match.arg(what)

  if (!is(preddata, "rcData"))
    preddata <- makePredData(preddata, object = object)

  concpred <- predict.rcgam(object, newdata = preddata, retransform = TRUE,
                            type = "response", ...)


}






