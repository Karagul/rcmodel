#' Rating-curve generalized additive model
#'
#' Fits generalized additive models to log-log relationships using `mgcv::gam`,
#' but incorporates aspects
#' specific to hydrologic rating curves. Specifically, tracks the geometric mean of
#' flow and concentration, adjusts for transformation bias, and calculates relevant
#' fit statistics such as Nash-Sutcliffe efficiency (NSE)
#'
#' Dots get passed to mgcv::gam() call.
#' All logarithms are base e (natural log)
#' Arguments ending in "var" are names of objects that will be assigned in the function.
#' logflowvar and logcvar will be shifted to have mean zero. POSSIBLY CHANGE THIS?
#'
#' @param formula a GAM formula. See `help("gam", package = "mgcv")` for details.
#' @param data an object of class rcData, generated using `makeModelData`, or a data frame that makeModelData can use.
#' @param ... further arguments passed to `mgcv::gam`
#' @export


rcgam <- function(formula, data, ...) {
  cl <- match.call()
  if (!is(data, "rcData"))
    data = makeModelData(data)
  formula = as.formula(formula)

  #   browser()
  out = R.utils::withTimeout(mgcv::gam(formula = formula, data = data, ...), timeout = 1,
                             onTimeout = "error")
  out$call = cl


  al = attributes(data)
  conc = al$transform$cinvert(data$c)
  conc.pred = al$transform$cinvert(out$fitted.values)
  scoef = mean(conc) / mean(conc.pred)
  if (scoef > 1.5) message(paste0("smearing coefficient is high: ", scoef))

  fitted.retrans = conc.pred * scoef

  resid.retrans = conc - fitted.retrans
  NSE = 1 - sum(resid.retrans ^ 2) / sum((conc - mean(conc))^2)

  newbits = list(data = data,
                 stats = c(al$stats),
                 yname = "conc",
                 smearCoef = scoef,
                 transform = al$transform,
                 units = al$units,
                 fitted.retrans, resid.retrans = resid.retrans, NSE = NSE)

  out = c(out, newbits)
  structure(out, class = c("rcgam", "gam", "glm", "lm"))
}


#' Predict method for rcgam fits
#'
#' Predict values using an rcgam model object
#'
#' @param object An rcgam object to use for predicting
#' @param newdata a data.frame containing precictor variables to use for prediction
#' @param retransform Should the predictions be returned as concentrations? (defaults to TRUE)
#' @param ... Arguments passed to `predict.gam` function call
#' @param smear Use Smearing estimator to correct transformation bias?
#'


predict.rcgam <- function(object, newdata, flowcol = "flow",
                          smear = TRUE, retransform = TRUE,
                          what = c("conc_mg.l", "load_kg.d"), ...) {


  what = match.arg(what)

  tfm <- object$transform
  if (missing(newdata))
    newdata = getData(object, type = "raw")
    if(!all(newdata$flow.units == "CFS"))
      stop("units other than CFS not currently supported")
    if(!all(newdata$conc.units %in% c("mg/L", "mg/l")))
      stop("units other than mg/L not currently supported")
    newdata <- newdata %>%
      mutate_(q = ~ tfm$qtrans(newdata[[flowcol]]),
              time = ~ tfm$ttrans(as.numeric(Date)),
              doy = ~ as.numeric(format(Date, "%j")))
  # }

  preds = mgcv::predict.gam(object = object, newdata = newdata, ...)

  if(!retransform)
    return(preds)

  if (smear) {
    if (is.list(preds))
      preds$fit = object$transform$cinvert(preds$fit) * object$smearCoef
    else preds = object$transform$cinvert(preds) * object$smearCoef
  } else {
    if (is.list(preds))
      preds$fit = object$transform$cinvert(preds$fit)
    else preds = object$transform$cinvert(preds)
  }
  if(what == "load_kg.d")
    preds = calcLoad(flow = newdata[[flowcol]], conc = preds)
  preds
}


#' @export
condlSample.rcgam <- function(object, newdata, flowcol = "flow",
                              flow.units = "CFS", quantile) {

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

  preds = condlSample.lm(object = object, newdata = newdata,
                         quantile = quantile, smear = FALSE, retransform = FALSE)
  preds = object$transform$cinvert(preds)
  preds
}

