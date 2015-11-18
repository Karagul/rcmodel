


#' Conduct a differential split sample test on a regression model
#'
#' Returns prediction errors from a model calibrated for condition == FALSE and validated
#' for condition == TRUE. Based on differential split sample test described in
#' Klemes (1986)
#'
#' @param object an rcgam object
#' @param condition a condition that returns logical when given a row of the
#' default newdata for predict(object)
#' @param what What to obtain errors for (concentration or load)
#' @param retransform Perform retransformation before calculating errors?
#' @param scale How to scale the residuals? (For comparing multiple models
#' for different datasets) Defaults to "none".
#' @param kfolds For scale == "cv" only. How many folds to use for crossvalidation?
#' @param incl.data Return data and scale along with residuals? If so, a list is returned.
#' @param ... passed to predict.rcgam
#' @importFrom markstats getData crossvalidate
#' @export

splitSampleTest <- function(object, condition,
                            what = c("conc_mg.l", "load_kg.d"),
                            retransform = TRUE,
                            scale = c("none", "gcv", "cv"),
                            kfolds = 10, incl.data = FALSE,
                            ...) {
  what = match.arg(what)
  scale = match.arg(scale)
  if(what == "load_kg.d" && ! retransform)
    stop("Loads must be crossvalidated in original units")
  data1 <- getData(object, type = "raw")
  data2 <- getData(object, type = "rcData")
  data <- cbind(data1, data2[setdiff(names(data2), names(data1))])

  data$load = calcLoad(data$conc, data$flow)
  fmla <- object$formula

  yname <- ifelse(what == "conc_mg.l", ifelse(retransform, "conc", "c"),
                  "load")
  # browser()
  split <- eval(substitute(condition), data)
  train <- data[!split, ]
  test <- data[split, ]
  if(sum(split) == 0) {
    curobj = object
    out = numeric(0)
    denom = NA
  } else {

    curobj <- do.call("rcgam", list(formula = fmla, data = train))
    ypred <- as.numeric(predict(curobj, newdata = test, what = what,
                                retransform = retransform, ...)$fit)
    ymeas <- test[[yname]]

    if (scale == "gcv") {
      if(retransform)
        stop("gcv scaling only works on transformed values")
      denom <- sqrt(curobj$gcv.ubre)
    } else if (scale == "cv") {
      message("crossvalidating")
      denom <- crossvalidate(curobj, statistic = "rmse", kfolds = kfolds,
                             what = what, retransform = retransform)
    } else
      denom <- 1
    ypred <- ypred / denom
    ymeas <- ymeas / denom

    out <- ymeas - ypred
  }

  gof = list(NSE = curobj$NSE,
             R2  = markstats::R2(curobj),
             Q2  = markstats::Q2(curobj))

  if (incl.data)
    out <- list(resid = out, data = test, scale = setNames(denom, scale), gof = gof)
  out
}
