#' @export

crossvalidate <- function(object, ...) {
  UseMethod("crossvalidate")
}


#' Crossvalidation for rcgam models
#'
#' @param ... Passed to predict.rcgam
#'
#' @export
crossvalidate.rcgam  <- function(object, kfolds = 0,
                                 statistic = c("R2", "mse", "mae", "rmse"),
                                 ...) {

  data <- getData(object)
  fmla <- object$formula

  if(kfolds == 0)
    kfolds <- nrow(data)

  yname <- object$yname # sneaky back door for objects of my own design

  yvar <- var(data[[yname]])

  statistic = match.arg(statistic)
  sfun = ifelse(statistic == "mae", "mae", "sse")

  # split data into folds
  case.folds = sample(rep(1:kfolds, length.out = nrow(data)))
  # fold.ss = rep(NA_real_, kfolds) # sum of squared residuals for each fold
  # fold.mae = rep(NA_real_, kfolds) # mean absolute error for each fold
  fold.stat = rep(NA_real_, kfolds) # mean absolute error for each fold

  for (fold in 1:kfolds) {
    train <- data[case.folds != fold, ]
    test <- data[case.folds == fold, ]
    curobj <- do.call("rcgam", list(formula = fmla, data = train))

    ypred <- as.numeric(predict(curobj, newdata = test, ...))
    ymeas <- test[[yname]]

    # fold.ss[fold] <- sum((ymeas - ypred)^2)
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

  sagg
}
