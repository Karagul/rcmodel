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
#' @param timeout the amount of time in seconds to try mgcv::gam before stopping with a timeout error
#' @param ... further arguments passed to `mgcv::gam`
#' @export


rcgam <- function(formula, data, timeout = 1, ...) {
  cl <- match.call()
  if (!is(data, "rcData"))
    data = makeModelData(data)
  formula = as.formula(formula)

  out = R.utils::withTimeout(mgcv::gam(formula = formula, data = data, ...), timeout = timeout,
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
                 fitted.retrans = fitted.retrans,
                 resid.retrans = resid.retrans, NSE = NSE)

  out = c(out, newbits)
  structure(out, class = c("rcgam", "gam", "glm", "lm"))
}

#' Predict method for rcgam model objects
#'
#' @param object an object of type rcgam, with which to make predictions
#' @param smear Unbias the predictions using the smearing coefficient? Defaults to TRUE
#' @param retrans Retransform the predictions into the original units? Defaults to TRUE
#' @param type What to predict. If not "response" (the default), then the following are coerced to be FALSE:
#' retrans, restrict, smear. See ?predict.gam.
#' @param restrict Restrict the range of predictions to those observed in the calibration data? Defaults to FALSE.
#' @param ... Other arguments passed to mgcv::predict.gam
#'
#'
#' @return  a list with element "fit" and potentially others,
#' depending on optional arguments passed. See help("predict.gam", package = "mgcv") for more info
#' The difference is that here the returned value will always be a list, possibly of length 1.
#' @export
predict.rcgam <- function(object, smear = TRUE, retransform = TRUE,
                          type = "response", restrict = FALSE, ...) {
  arglist = list(...)
  if("newdata" %in% names(arglist) && !is(arglist$newdata, "rcData"))
    arglist$newdata = makePredData(arglist$newdata, object = object)

  predfun <- get("predict.gam", asNamespace("mgcv"))
  out <- do.call("predfun", args = c(list(object = object, type = type), arglist))

  if(type != "response") {
    if(smear || retransform || restrict)
      warning("The following arguments are not allowed to be TRUE when type != 'response': smear, retransform, restrict")
    return(out)
  }


  if (!is(out, "list"))
    out = list(fit = out)
  if (restrict) {
    cmax <- max(object$model$c)
    cmin <- min(object$model$c)
    out$fit[out$fit > cmax] <- cmax
    out$fit[out$fit < cmin] <- cmin
  }
  if (retransform){
    out$fit = object$transform$cinvert(out$fit)
    if (smear)
      out$fit = out$fit * object$smearCoef
  } else if(smear)
    warning("smearing not applicable with non-retransformed predictions. Ignoring this argument.")
  out
}

#' Convert an rcgam to a gam
#' Useful for applying gam-specific methods that may not work with e.g. predict.rcgam
#' @export
as.gam <- function(object) {
  stopifnot(is(object, "rcgam"))

  toRemove <- c("data", "stats", "yname", "smearCoef", "transform", "units",
                "fitted.retrans", "resid.retrans", "NSE")
  out <- within.list(object, rm(list = toRemove))
  nulls <- vapply(out, is.null, logical(1))
  out <- structure(out[!nulls], class = c("gam", "glm", "lm"))
  out
}
