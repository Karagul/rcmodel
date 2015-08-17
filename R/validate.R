#' @export

crossvalidate <- function(object, ...) {
  UseMethod("crossvalidate")
}


#' Crossvalidation for rcgam models
#'
#' @param what what quantity to predict/validate
#' @param ... Passed to predict.rcgam
#'
#' @export
crossvalidate.rcgam  <- function(object, kfolds = 0,
                                 statistic = c("R2", "mse", "mae", "rmse"),
                                 what = c("conc_mg.l", "load_kg.d"), ...) {
  what = match.arg(what)
  data <- getData(object)
  data$load = calcLoad(data$conc, data$flow)
  fmla <- object$formula


  yname <- ifelse(what == "conc_mg.l", "conc", "load") # sneaky back door for objects of my own design
  statistic = match.arg(statistic)
  sfun = ifelse(statistic == "mae", "mae", "sse")

  if(kfolds == 0) {
    preds = rep(NA_real_, nrow(data))
    for(fold in 1:nrow(data)) {
      train <- data[-fold, ]
      test <- data[fold, ]
      curobj <- do.call("rcgam", list(formula = fmla, data = train))

      ypred <- as.numeric(predict(curobj, newdata = test, what = what, ...))
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

      ypred <- as.numeric(predict(curobj, newdata = test, what = what, ...))
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
#' @param What to obtain errors for (concentration or load)
#' default newdata for predict(object)
#' @param ... passed to predict.rcgam
#'

splitSampleTest <- function(object, condition,
                            what = c("conc_mg.l", "load_kg.d"),
                            ...) {
  what <- match.arg(what)
  stopifnot(is(object, "rcgam"))
  data <- getData(object)

  split <- eval(substitute(condition), data)
  train <- data[!split, ]
  test <- data[split, ]

  fmla <- object$formula
  yname <- ifelse(what == "conc_mg.l", "conc", "load")

  curobj <- do.call("rcgam", list(formula = fmla, data = train))

  ypred <- as.numeric(predict(curobj, newdata = test, what = what, ...))
  ymeas <- test[[yname]]

  out <- ymeas - ypred
  out
}

