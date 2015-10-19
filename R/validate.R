
#' Crossvalidation for rcgam models
#'
#' @param what what quantity to predict/validate
#' @param retransform compare predictions to observations in original uints?
#' Must be TRUE if what == "load_kg.d"
#' @param ... Passed to predict.rcgam
#'
#' @export
#' @importFrom markstats crossvalidate

crossvalidate.rcgam  <- function(object, kfolds = 0,
                                 statistic = c("R2", "mse", "mae", "rmse"),
                                 what = c("conc_mg.l", "load_kg.d"),
                                 retransform = TRUE, ...) {
  what = match.arg(what)
  if(what == "load_kg.d" && ! retransform)
    stop("Loads must be crossvalidated in original units")
  data <- getData(object)
  data$c <- object$transform$ctrans(data$conc)
  data$load = calcLoad(data$conc, data$flow)
  fmla <- object$formula

  yname <- ifelse(what == "conc_mg.l", ifelse(retransform, "conc", "c"),
                  "load")
  statistic = match.arg(statistic)
  sfun = ifelse(statistic == "mae", "mae", "sse")

  if(kfolds == 0) {
    preds = rep(NA_real_, nrow(data))
    for(fold in 1:nrow(data)) {
      train <- data[-fold, ]
      test <- data[fold, ]
      curobj <- do.call("rcgam", list(formula = fmla, data = train))

      ypred <- as.numeric(predict(curobj, newdata = test, what = what,
                                  retransform = retransform, ...))
      ymeas <- test[[yname]]
      preds[fold] = ypred
    }
    stat1 = do.call(sfun, list(data[[yname]], preds))

    if(statistic == "mae")
      sagg = stat1
    else if (statistic == "mse")
      sagg = stat1 /  nrow(data)
    else if (statistic == "rmse")
      sagg = sqrt(stat1 / nrow(data))
    else
      sagg = 1 - stat1 / sum((data[[yname]] - mean(data[[yname]], na.rm = TRUE))^2)
  } else {
    # split data into folds
    case.folds = sample(rep(1:kfolds, length.out = nrow(data)))

    fold.stat = rep(NA_real_, kfolds) # error statistic for each fold

    for (fold in 1:kfolds) {
      train <- data[case.folds != fold, ]
      test <- data[case.folds == fold, ]
      curobj <- do.call("rcgam", list(formula = fmla, data = train))

      ypred <- as.numeric(predict(curobj, newdata = test, what = what,
                                  retransform = retransform, ...))
      ymeas <- test[[yname]]
      fold.stat[fold] <- do.call(sfun, list(ymeas, ypred))
    }

    # assemble folds
    # r2 <- 1 - sum(fold.ss) / sum((ymeas - mean(ymeas, na.rm = TRUE))^2)
    ymeas <- data[[yname]]
    ssum <- sum(fold.stat)

    if(statistic == "mae")
      sagg = ssum / kfolds
    else if (statistic == "mse")
      sagg = ssum / nrow(data)
    else if (statistic == "rmse")
      sagg = sqrt(ssum / nrow(data))
    else
      sagg = 1 - ssum / sum((ymeas - mean(ymeas, na.rm = TRUE))^2)
  }
  sagg
}


#' Conduct a differential split sample test on a regression model
#'
#' Returns prediction errors from a model calibrated for condition == FALSE and validated
#' for conditiona == TRUE. Based on differential split sample test described in
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
#' @export

splitSampleTest <- function(object, condition,
                            what = c("conc_mg.l", "load_kg.d"),
                            retransform = TRUE,
                            scale = c("none", "gcv", "cv"),
                            kfolds = 30, incl.data = FALSE,
                            ...) {
  what = match.arg(what)
  scale = match.arg(scale)
  if(what == "load_kg.d" && ! retransform)
    stop("Loads must be crossvalidated in original units")
  data1 <- getData(object)
  data2 <- getData(object, type = "rcData")
  data <- cbind(data1, data2[setdiff(names(data2), names(data1))])

  data$load = calcLoad(data$conc, data$flow)
  fmla <- object$formula

  yname <- ifelse(what == "conc_mg.l", ifelse(retransform, "conc", "c"),
                  "load")

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

  tss = sum((curobj$y - mean(curobj$y))^2)
  gof = list(NSE = curobj$NSE,
             R2 = 1 - sum(residuals(curobj, type = "response")^2) / tss,
             R2_gcv = 1 - curobj$gcv.ubre * nrow(train) / tss)

  if (incl.data)
    out <- list(resid = out, data = test, scale = setNames(denom, scale), gof = gof)
  out
}
