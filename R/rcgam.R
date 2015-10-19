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

#' Predict method for rcgam model objects
#'
#' @return  a list with element "fit" and potentially others,
#' depending on optional arguments passed. See help("predict.gam", package = "mgcv") for more info
#' The difference is that here the returned value will always be a list, possibly of length 1.
#' @export
predict.rcgam <- function(object, smear = TRUE, retrans = TRUE, ...) {
  arglist = list(...)
  if("newdata" %in% names(arglist) && !is(arglist$newdata, "rcData"))
    arglist$newdata = makePredData(arglist$newdata, object = object)

  predfun <- get("predict.gam", asNamespace("mgcv"))
  out = do.call("predfun", args = c(list(object = object), arglist))
  if(!is(out, "list"))
    # browser()
    out = list(fit = out)
  if (retrans){
    out$fit = object$transform$cinvert(out$fit)
    if (smear)
      out$fit = out$fit * object$smearCoef
  } else if(smear)
    warning("smearing not applicable with non-retransformed predictions. Ignoring this argument.")
  out
}

