#' Rating-curve generalized additive model
#'
#' Fits generalized additive models to log-log relationships using `mgcv::gam`,
#' but incorporates aspects
#' specific to hydrologic rating curves. Specifically, tracks the geometric mean of
#' flow and concentration, adjusts for transformation bias, and calculates relevant
#' fit statistics such as Nash-Sutcliffe efficiency (NSE)
#'
#' Dots get passed to gam() call.
#' All logarithms are base e (natural log)
#' Arguments ending in "var" are names of objects that will be assigned in the function.
#' logflowvar and logcvar will be shifted to have mean zero. POSSIBLY CHANGE THIS?
#'
#' @param formula a GAM formula. See `help("gam", package = "mgcv")` for details.
#' @param data an object of class rcData, generated using `makeModelData`
#' @param ... further arguments passed to `mgcv::gam`
#' @export


rcgam <- function(formula, data, ...) {
  cl <- match.call()
  if(!is(data, "rcData")) stop("'data' must be an object of class 'rcData'. Use 'makeModelData()` to generate.")
  formula = as.formula(formula)

  dates = data[["Date"]]
  datebar = mean(dates)

  data[["time"]] = as.numeric(dates) - as.numeric(datebar)
  data[["doy"]] = as.numeric(format(dates, "%j"))
  #   browser()
  out = gam(formula = formula, data = data, ...)
  out$call = cl
  al = attributes(data)
  conc = al$transform$cinvert(data$c)
  conc.pred = al$transform$cinvert(out$fitted.values)
  scoef = mean(conc) / mean(conc.pred)
  if(scoef > 1.5) message(paste0("smearing coefficient is high: ", scoef))

  fitted.retrans = conc.pred * scoef

  resid.retrans = conc - fitted.retrans
  NSE = 1 - sum(resid.retrans ^ 2) / sum((conc - mean(conc))^2)

  newbits = list(stats = c(al$stats, datebar = datebar),
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
#' @param ... Arguments passed to `predict.gam` function call
#' @param smear Use Smearing estimator to correct transformation bias?
#'


predict.rcgam <- function(object, newdata, flowcol = "flow",
                          flow.units = "CFS", ..., smear = TRUE, quantile = NULL) {

  newdata <- newdata %>%
    mutate_(q = ~ object$transform$qtrans(newdata[[flowcol]]),
            time = ~ as.numeric(Date) - as.numeric(object$stats["datebar"]),
            doy = ~ as.numeric(format(Date, "%j")))

  preds = as.data.frame(predict.gam(object = object, newdata = newdata, ...))
  names(preds)[1] = "fit"
  if(!is.null(quantile))
    preds$fit = condlSample(object = object, newdata = newdata, quantile = quantile)
  }

  # unbias the retransformed estimates
  if(smear) {
    preds$fit = object$transform$cinvert(preds$fit) * object$smearCoef
  }

  out = cbind(newdata, preds)
}

# testing

load("data/sampleData.rda")
foo = makeModelData(sampleData)
mod1 = rcgam(c ~ s(q), foo)
summary(mod1)
plot(mod1, all.terms = TRUE, residuals = TRUE)
termplot(mod1)

plot(b, all.terms = TRUE)

bar = predict(mod1, newdata = sampleData)

mod2 = rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), foo)
summary(mod2)
plot(mod2, pages = 1)

pred2 = predict(mod2, sampleData, se.fit = TRUE)
plot(fit ~ conc, pred2)
mod2$NSE
