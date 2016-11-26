#' Generate samples from a conditional distribution obtained from a model prediction
#'
#' @param object An rclm or rcgam object to use for predicting
#' @param newdata a data.frame containing precictor variables to use for prediction
#' @param retransform Should the predictions be returned as concentrations? (defaults to TRUE)
#' @param ... Arguments passed to `predict.lm` or predict.gam function call
#' @param smear Use Smearing estimator to correct transformation bias?
#' @describeIn condlSample Method for rcgam objects
#' @importFrom dplyr "%>%" mutate_
#' @importFrom markstats condlSample
#' @export
condlSample.rcgam <- function(object, newdata, flowcol = "flow",
                              flow.units = "ft3/s", quantile, ...) {

  if (!requireNamespace("rcmodel", quietly = TRUE)) {
    stop("rcmodel needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # library("mgcv")
  if (missing(newdata))
    newdata = getData(object)

  assertthat::assert_that(is(newdata$Date, "Date"))
  assertthat::assert_that(flowcol %in% names(newdata))
  assertthat::assert_that("flow.units" %in% names(newdata))
  assertthat::assert_that(all(as.character(newdata$flow.units) == object$units["qunits"]))

  newdata <- newdata %>%
    mutate_(q = ~ object$transform$qtrans(newdata[[flowcol]]),
            time = ~ as.numeric(Date) - as.numeric(object$stats["datebar"]),
            doy = ~ as.numeric(format(Date, "%j")))
  preds = NextMethod("condlSample", object = object, newdata = newdata,
                     quantile = quantile, smear = FALSE, retransform = FALSE,
                     ...)
  #   preds = condlSample.lm(object = object, newdata = newdata,
  #                          quantile = quantile, smear = FALSE, retransform = FALSE)
  preds = object$transform$cinvert(preds)
  preds
}


#' Generate samples from a conditional distribution obtained from a model prediction
#'
#' @param object An rclm or rcgam object to use for predicting
#' @param newdata a data.frame containing precictor variables to use for prediction
#' @param retransform Should the predictions be returned as concentrations? (defaults to TRUE)
#' @param ... Arguments passed to `predict.lm` or predict.gam function call
#' @param smear Use Smearing estimator to correct transformation bias?
#' @describeIn condlSample Method for rclm objects
#' @importFrom dplyr "%>%" mutate_
#' @importFrom markstats condlSample
#' @export
condlSample.rclm <- function(object, newdata, flowcol = "flow",
                             flow.units = "ft3/s", quantile, ...) {

  if (!requireNamespace("rcmodel", quietly = TRUE)) {
    stop("rcmodel needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # library("mgcv")
  if (missing(newdata))
    newdata = getData(object)

  assertthat::assert_that(is(newdata$Date, "Date"))
  assertthat::assert_that(flowcol %in% names(newdata))
  assertthat::assert_that("flow.units" %in% names(newdata))
  assertthat::assert_that(all(as.character(newdata$flow.units) == object$units["qunits"]))

  newdata <- newdata %>%
    mutate_(q = ~ object$transform$qtrans(newdata[[flowcol]]),
            time = ~ as.numeric(Date) - as.numeric(object$stats["datebar"]),
            doy = ~ as.numeric(format(Date, "%j")))
  preds = NextMethod("condlSample", object = object, newdata = newdata,
                     quantile = quantile, smear = FALSE, retransform = FALSE,
                     ...)
  #   preds = condlSample.lm(object = object, newdata = newdata,
  #                          quantile = quantile, smear = FALSE, retransform = FALSE)
  preds = object$transform$cinvert(preds)
  preds
}
