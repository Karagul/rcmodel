# functions from markstats package--to make rcmodel more self-contained


# from getData.R ----------------------------------------------------------

#' Get data from lm objects
#'
#' simple extraction of data, returning useful errors if impossible. Useful as a
#' method for the generic `getData` from `nlme` package
#'
#' @param object an object inheriting from class `lm`
#' @seealso getData.rcgam
#' @export
#'

getData <- function(object, ...) {
  UseMethod("getData")
}

#' @export
getData.lm <- function(object) {
  if (is.null(object$model)) {
    message("model structure does not include data. Attempting to get from environment")
    eval(object$call$data, envir = attr(object$terms, ".Environment"))
  }
  else
    object$model
}



# from ggfuns.R -----------------------------------------------------------


#' ggplot2 implementation of termplot with partial residuals, using visreg::visreg()
#' @param object A model object that visreg() recognizes
#' @param xvar Which term to inspect. Currently only works for one at a time.
#' @param data If object$call$data isn't in the model environment, it can be supplied using this
#' @importFrom visreg visreg
#' @importFrom ggplot2 ggplot geom_ribbon geom_line geom_point aes_
#' @export
ggTermPlot <- function(object, ...) {
  UseMethod("ggTermPlot")
}

#' @export
ggTermPlot.lm <- function(object, xvar, data = NULL, ...) {
  if (!is.null(data))
    object$call$data <- as.name("data")
  vis = visreg(object, xvar = xvar, plot = FALSE, ...)

  out <- ggplot(data = vis$fit) +
    geom_ribbon(aes_(x = as.name(xvar), ymin = ~visregLwr, ymax = ~visregUpr),
                alpha = 0.5) +
    geom_line(aes_(x = as.name(xvar), y = ~visregFit)) +
    geom_point(aes_(x = as.name(xvar), y = ~visregRes), data = vis$res)
  out
}


# from gofStats.R ---------------------------------------------------------

#' coefficient of determination
#'
#' @export
#'
R2 <- function(x, ...) {
  UseMethod("R2")
}


#' Default S3 method for coefficient of determination
#' @param x observed data
#' @param xpred predictions
#' @export
R2.default <- function(x, xpred) {
  stopifnot(length(x) == length(xpred))
  1 - sum((x - xpred)^2) / sum((x - mean(x))^2)
}

#' Coefficient of dermination for lm objects
#' Optionally gives adjusted R2.
#' @param x an object of class "lm"
#' @export
R2.lm <- function(x, adjust = FALSE) {
  if(adjust)
    out <- summary(x)$adj.r.squared
  else
    out <- summary(x)$r.squared
  out
}

#' Coefficient of dermination for gam objects
#' Optionally gives adjusted R2.
#' @param x an object of class "gam"
#' @export
R2.gam <- function(x, adjust = FALSE) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("mgcv needed for this function to work. Please install it.",
         call. = FALSE)
  }
  resid <- mgcv::residuals.gam(x, type = "response")
  if(adjust)
    out <- summary(x)$r.sq
  else
    out <- 1 - sum(resid^2) / sum((x$y - mean(x$y))^2)
  out
}


#' Leave-one-out coefficient of determination
#' Similar to R2, but uses leave-one-out estimates for response and mean.
#' Uses GCV and OCV to estimate MSE of prediction.
#' @param object an object with a class for `deviance`, `residuals`, and `hatvalues`.
#' @export
#'
Q2 <- function(object, method = c("gcv", "ocv")) {
  method = match.arg(method)
  mse <- ifelse(method == "gcv", gcv(object), ocv(object))
  1 - length(object$y) * mse / tss_loo(object$y)
}

#' mean absolute error
#' @param obs observed data
#' @param pred predictions
#'
#' @export
mae <- function(obs, pred) {
  stopifnot(length(obs) == length(pred))
  mean(abs(obs - pred))
}

#' sum of squared errors
#' @param obs observed data
#' @param pred predictions
#'
#' @export
sse <- function(obs, pred) {
  stopifnot(length(obs) == length(pred))
  sum((obs - pred)^2)
}


# from crossvalidate.R ----------------------------------------------------

#' Cross-validation of regression models
#'
#' Generic function that computes a crossvalidation score for regression models.
#'
#'
#' @param object a model object
#' @param kfolds Number of folds to use for crossvalidation. 0 (default) corresponds to leave-one-out crossvalidation.
#' @param statistic performance statistic to use.
#'
#' @export

crossvalidate <- function(object, ...) {
  UseMethod("crossvalidate")
}

#' @export

crossvalidate.lm <- function(object, kfolds = 0, statistic = c("R2", "mse", "mae", "rmse")) {

  data <- getData(object)
  yname <- object$yname # sneaky back door for objects of my own design
  curcall <- object$call
  if(is.null(yname))
    yname <- as.character(curcall$formula[[2]])
  ymeas <- data[[yname]]

  statistic = match.arg(statistic)
  sfun = ifelse(statistic == "mae", "mae", "sse")

  if(kfolds == 0) { # leave-one-out crossvalidation
    if(statistic != "mae") {
      mse <- ocv(object)
      if (statistic == "mse")
        return(mse)
      else if (statistic == "rmse")
        return(sqrt(mse))
      else
        return = 1 - mse / sum((ymeas - mean(ymeas, na.rm = TRUE))^2)
    }
    kfolds <- nrow(data)
  }

  # split data into folds
  case.folds = sample(rep(1:kfolds, length.out = nrow(data)))
  fold.stat = rep(NA_real_, kfolds) # mean absolute error for each fold

  for (fold in 1:kfolds) {
    train <- data[case.folds != fold, ]
    test <- data[case.folds == fold, ]
    curcall$data <- quote(train)
    curobj <- eval(curcall)

    ypred <- as.numeric(predict(curobj, newdata = test))
    ymeas <- test[[yname]]

    fold.stat[fold] <- do.call(sfun, list(ymeas, ypred))
  }

  # assemble folds
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



#' Ordinary Crossvalidation score
#' Following Wood (2006), p. 174
#' @param object a model object with methods for `residuals` and `hatvalues`.
#' @export
#'
ocv <- function(object) {
  hat <- stats::hatvalues(object)
  denom <- (1 - hat)^2
  ocv <- mean(residuals(object, type = "response")^2 / denom)
  ocv
}

#' Generalized Crossvalidation score
#' Following Wood (2006), p. 178
#' @param object a model object with methods for `deviance`, `residuals` and `hatvalues`.
#' @export
#'
gcv <- function(object) {
  hat <- hatvalues(object)
  n <- length(object$fitted.values)
  out <- n * deviance(object) / (n - sum(hat))^2
  out
}
