#' lm rating-curve model
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
#'
#' @export


rclm <- function(formula, data, ...,
                  remove.outliers = TRUE,
                  flowcol = "flow",
                  lqbar = attr(data, "lqbar"),
                  lqsd = attr(data, "lqsd"),
                  ccol = "concentration",
                  datecol = "Date",
                  logflowvar = "logQ",
                  logcvar = "logC",
                  doyvar = "doy",
                  numtimevar = "numTime") {

  formula = as.formula(formula)

  lq = log(data[[flowcol]])
  lc = log(data[[ccol]])
  stopifnot(!any(is.na(lq)) && !any(is.na(lc))) # check for missing data
  if(is.null(lqbar)) lqbar = mean(lq, na.rm = TRUE)
  if(is.null(lqsd)) lqsd = sd(lq, na.rm = TRUE)
  lcbar = mean(lc)

  dates = data[[datecol]]
  datebar = mean(dates)

  data[[logflowvar]] = lq - lqbar
  data[[logcvar]] = lc - lcbar
  data[[numtimevar]] = as.numeric(dates) - as.numeric(datebar)
  data[[doyvar]] = as.numeric(format(dates, "%j"))
  #   browser()
  out = lm(formula = formula, data = data, ...)
  if(remove.outliers){
    outliers = abs(residuals(out) / sd(residuals(out))) > 3
    if(sum(outliers) > 0) {
      message(paste0("removing ", sum(outliers), " outliers"))
      data = data[!outliers,]
      out = lm(formula = formula, data = data, ...)
    }
  }

  scoef = mean(data[[ccol]]) / mean(exp(out$fitted.values))
  if(scoef > 1.5) message(paste0("smearing coefficient is high: ", scoef))

  fitted.retrans = exp(out[["fitted.values"]]) * scoef

  resid.retrans = data[[ccol]] - fitted.retrans
  NSE = 1 - sum(resid.retrans ^ 2) / sum((data[[ccol]] - mean(data[[ccol]]))^2)

  newbits = list(lqbar = lqbar, lqsd = lqsd, lcbar = lcbar, datebar = datebar,
                 smearCoef = scoef, fitted.retrans, resid.retrans = resid.retrans,
                 NSE = NSE)

  out = c(out, newbits)
  structure(out, class = c("rclm", "lm", "glm", "lm"),
            flowcol = flowcol, ccol = ccol, datecol = datecol,
            logflowvar = logflowvar, logcvar = logcvar, doyvar = doyvar,
            numtimevar = numtimevar)
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


predict.rclm <- function(object, newdata, ..., smear = TRUE,
                          flowcol = attr(object, "flowcol"),
                          ccol = attr(object, "ccol"),
                          datecol = attr(object, "datecol"),
                          logflowvar = attr(object, "logflowvar"),
                          logcvar = attr(object, "logcvar"),
                          doyvar = attr(object, "doyvar"),
                          numtimevar = attr(object, "numtimevar")) {

  stopifnot(is(newdata[[datecol]], "Date") && is(object[["datebar"]], "Date"))

  # Make prediction columns
  newdata[[logflowvar]] = log(newdata[[flowcol]]) - object[["lqbar"]]
  newdata[[logcvar]] = log(newdata[[ccol]]) - object[["lcbar"]]
  newdata[[numtimevar]] = as.numeric(newdata[[datecol]]) - as.numeric(object[["datebar"]])
  newdata[[doyvar]] = as.numeric(format(newdata[[datecol]], "%j"))

  # make necessary columns
  for(term in attr(object$terms, "term.labels")) {
    newdata[[term]] = with(newdata, eval(parse(text = term)))
  }

  preds = as.data.frame(predict.lm(object = object, newdata = newdata, ...))
  names(preds)[1] = "fit"

  # unbias the retransformed estimates
  if(smear) {
    preds$fit = exp(preds$fit) * object$smearCoef
  }

  preds$residual = newdata[[ccol]] - preds[["fit"]]
  denom = exp(sd(residuals(object, type = "response"))) # Is this proper scaling?
  print(paste("denom:", denom))
  preds$scaled_resid = preds$residual / denom
  #   browser()
  preds$aR2 = summary(object)[["r.sq"]]

  #   newdata$qdist = newdata$stLogFlow
  out = data.frame(newdata, setNames(Reduce(data.frame, preds), names(preds)))
}
