#' Compute seasonal harmonic functions
#'
#' Returns orthogonal harmonic functions from degree 1 to `degree` over a
#' specified set of dates, such that all have a common period of 1 year.
#' Based on the `stats::poly` function.
#'
#' returns a matrix with (2 * degree) columns, representing the powers-of-2 multiples of
#' sin and cosine functions with 1-year period.
#'
#' @param t a Date or POSIXt vector at which to evaluate the harmonic series.
#' @param degree the degree of the highest-degree harmonic function
#' @details Useful for adding seasonal harmonic terms in regression functions.
#' Uses 1 January as origin. Incorporates leap days exactly as `lubridate::leap_year()`
#' @return An object of class "sharm", which is a matrix with a `degree` attribute
#' corresponding to the maximum degree of the harmonic series.
#' @export

sharm <- function(t, degree = 1) {
  if (!is(t, "Date") && !is(t, "POSIXt"))
    stop("t must be a Date or time object")
  if (degree < 1)
    stop("'degree' must be at least 1")
  if (anyNA(t))
    stop("missing values are not allowed in 'sharm'")
  n <- degree + 1

  year = as.numeric(format(t, "%Y"))
  isly = (year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0)) # Thanks Hadley!
  denom = ifelse(isly, 15811200, 15768000)

  degs = 2L^((1L:degree) - 1)

  sins = matrix(vapply(degs, function(x) sinpi(x * jsec(t) / denom),
                       numeric(length(t))),
                ncol = degree)
  colnames(sins) = paste0("sin", degs)
  coss = matrix(vapply(degs, function(x) cospi(x * jsec(t) / denom),
                       numeric(length(t))),
                ncol = degree)
  colnames(coss) = paste0("cos", degs)

  Z <- cbind(sins, coss)
  attr(Z, "degree") <- degree
  class(Z) <- c("sharm", "matrix")

  Z
}

#' obtain "julian seconds" (number of seconds since start of the year)
#` @export
jsec = function(t) {
  vapply(t,
         function(x) eval(parse(text = format(x, "(%j - 1) * 3600 * 24 + %H * 3600 + %M * 60 + %S"))),
         numeric(1)
  )
}
