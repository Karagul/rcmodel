#' Linear Rating-curve model
#'
#' Fits linear models to log-log relationships using `mgcv::lm`,
#' but incorporates aspects
#' specific to hydrologic rating curves. Specifically, tracks the geometric mean of
#' flow and concentration, adjusts for transformation bias, and calculates relevant
#' fit statistics such as Nash-Sutcliffe efficiency (NSE)
#'
#' Dots get passed to lm() call.
#' All logarithms are base e (natural log)
#' Arguments ending in "var" are names of objects that will be assigned in the function.
#' logflowvar and logcvar will be shifted to have mean zero. POSSIBLY CHANGE THIS?
#'
#' @param formula a lm formula. See `help("lm")` for details.
#' @param data an object of class rcData, generated using `makeModelData`, or a data frame that makeModelData can use.
#' @param timeout the amount of time in seconds to try mgcv::gam before stopping with a timeout error
#' @param ... further arguments passed to `mgcv::gam`
#' @export


rclm <- function(formula, data, ...) {

  formula = as.formula(formula)

  if (!is(data, "rcData"))
    data = makeModelData(data)

  out = lm(formula = formula, data = data, ...)

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
  structure(out, class = c("rclm", "lm"))
}


#' Predict method for rclm fits
#'
#' Predict values using an rclm model object
#'
#' @param object An rclm object to use for predicting
#' @param newdata a data.frame containing precictor variables to use for prediction
#' @param ... Arguments passed to `predict.lm` function call
#' @param smear Use Smearing estimator to correct transformation bias?
#'


predict.rclm <- function(object, smear = TRUE, retransform = TRUE, restrict = FALSE, ...) {
  arglist = list(...)
  if("newdata" %in% names(arglist) && !is(arglist$newdata, "rcData"))
    arglist$newdata = makePredData(arglist$newdata, object = object)

  predfun <- get("predict.lm", asNamespace("stats"))
  out <- do.call("predfun", args = c(list(object = object, type = "response"), arglist))

  if (!is(out, "list"))
    out = list(fit = out)
  else
    out = out[1:2]
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
