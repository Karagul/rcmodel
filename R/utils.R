# Utility functions

#' ripped off from http://stackoverflow.com/a/8189441
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#' Simple timing of function execution
#'
#' Time the execution of a function, returning the function's output and messaging the time difference
#' @param expr Unevaluated function call.
#' @export

timeit <- function(expr) {
  t1 <- Sys.time()
  out <- evalq(expr)
  t2 <- Sys.time()
  time <- difftime(t2, t1, units = "auto")
  message(sprintf("Finished in %.4g %s.", time, attr(time, "units")))
  attr(out, "timeElapsed") <- time
  out
}


#' Convert timezones of a datetime character vector
#'
#' Conversion for a single timezone
#'
#' toUTC_oneTZ allows conversion for observations in a single timezone to UTC,
#' while toUTC allows for multiple starting timezones to be specified.
#' @param timestring A character vector convertible to POSIXct format
#' @param tzstring A character vector specifying time zones of timestring.
#' For toUTC_oneTZ, this must be of length one. For toUTC, this must have length
#' equal to that of timestring.
#' @param ... Arguments passed to as.POSIXct. (e.g. \code{format})
#' @seealso \code{\link{timezones}} for how to specify timezones, \code{\link{as.POSIXct}}
#' @export

toUTC_oneTZ <- function(timestring, tzstring, ...) {
  stopifnot(length(tzstring) == 1)
  psx1 <- as.POSIXct(timestring, tz = tzstring, ...)
  psx2 <- as.POSIXct(format(psx1, tz = "UTC", usetz = TRUE), tz = "UTC")
  psx2
}

#' @export
#' @describeIn toUTC_oneTZ Conversion allowing for multiple timezones

toUTC <- function(timestring, tzstring, ...) {
  stopifnot(length(timestring) == length(tzstring))
  tzstring <- as.factor(tzstring)
  timelist <- split(timestring, f = tzstring)

  mapfun <- function(timestring, tzstring) {
    out <- toUTC_oneTZ(timestring = timestring, tzstring = tzstring, ...)
  }

  utcList <- Map(mapfun, timestring = timelist, tzstring = levels(tzstring))
  out <- unsplit(utcList, f = tzstring)

  out
}

