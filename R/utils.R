# Utility functions

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


#' discount factor described in Wang et al., 2011
#'
#' Applies an exponential smooth function with parameter p
#'
#' @param x timeseries to exponentially smooth
#' @param d parameter of smooth. Must be strictly between 0 and 1. See Wang et al., 2011
#' @export
#'

discount <- function(x, d) {
  stopifnot(d > 0 && d < 1)

  dnumer <- Reduce(function(a, b) d * (a + b / d), d * x, accumulate = TRUE)
  dd <- d^(1 : (length(x)))
  ddenom <- cumsum(dd)

  out <- dnumer / ddenom
  out
}

#' Calculate antecedent dry days
#'
#' @param x Daily time series of rainfall amount.
#' @param thresh threshold precipitation amount for determining what is "dry"

adry <- function(x, thresh) {
  below <- x < thresh

  foo <- rle(as.numeric(below))
  dd <- foo$values == 1
  dds <- lapply(foo$lengths[dd], function(x) seq(1:x))
  wets <- lapply(foo$lengths[!dd], function(x) rep(0, x))

}

#' replacement function for commonly given, but "improper" units,
#' e.g. "CFS", which *should* be ft3/s
#'
#' Unless udunits2 is available, this only converts common concentration, flow,
#' and load units.
#'
#' @export
validateUnits <- function(unit,
                          # use.ud = requireNamespace("udunits2", quietly = TRUE)) {
                          use.ud = FALSE) {
  # unit <- tolower(unit)
  replacement <- c("cfs" = "ft3/s", "CFS" = "ft3/s",
                   "gpm" = "gallon/min",
                   "gal/min" = "gallon/min",
                   "mgd" = "Mgallon/day", "MGD" = "Mgallon/day")

  matches <- match(unit, names(replacement))
  matchna <- is.na(matches)
  out <- unit
  out[!matchna] <- replacement[matches[!matchna]]

  if (use.ud) {
    convertible <- vapply(out, udunits2::ud.is.parseable, logical(1))
  } else {
    convertible <- tolower(out) %in% tolower(unitTable$unit)
  }

  if (sum(!convertible) > 0)
    warning(paste("Unrecognized units:",
                  paste(unique(out[!convertible]), collapse = ", ")))

  out
}


#' Wrapper for ud.convert that accepts vector inputs and multiple conversion candidates.
#'
#' if udunits2 package is available, this is used. Otherwise my own functions.
#'
#' @param x Numeric vector of values
#' @param from Units of x values
#' @param to Units to convert to.
#' @param inconvertibles Should units that cannot be converted be preserved
#' or omitted from the result?
#' @return A data.frame with columns x and units
#' @details If length(to) > 0, units the first is tried first, followed by the second, etc.
#'  If units in from are already contained in to, these units are not converted.
#' @importFrom assertthat assert_that
#' @export

convertUnits <- function(x, from, to, inconvertibles = c("preserve", "omit"),
                         # use.ud = requireNamespace("udunits2", quietly = TRUE)) {
                         use.ud = FALSE) {

  inconvertibles <- match.arg(inconvertibles)

  if (length(from) == 1)
    from <- rep(from, length(x))
  if (length(to) == 1)
    to <- rep(to, length(x))

  assert_that(length(x) == length(from),
              length(x) == length(to),
              is(from, "character"),
              is(to, "character"))

  from <- validateUnits(from)
  to <- validateUnits(to)


  if (use.ud) {
    out <- convertUnits_ud(x = x, from = from, to = to, inconvertibles = inconvertibles)
  } else {
    out <- convertUnits_r(x = x, from = from, to = to, inconvertibles = inconvertibles)
  }

  out

}

convertUnits_ud <- function(x, from, to, inconvertibles = c("preserve", "omit")) {

  inconvertibles <- match.arg(inconvertibles)

  # Perform a SINGLE conversion with nice error handling
  conv <- function(to_) {
    ret1 <- function(x_, from_) {
      ret2 <- try(udunits2::ud.convert(x_, from_, to_), silent = TRUE)
      ret2 <- ifelse(is(ret2, "try-error"), NA_real_, ret2)
    }
    ret1
  }

  # vectorized conversion
  conv2 <- function(x_, from_, to_) {
    unlist(Map(conv(to_), x_ = x_, from_ = from_))
  }

  inds <- is.na(match(from, to)) # indices of original units vector that don't match conversion candidates
  out <- data.frame(x = x, units = from, stringsAsFactors = FALSE)

  # map unique(from) to unit from to
  ufrom <- unique(from)
  whichCanConvert <- function(tounit)
    ufrom[vapply(ufrom, udunits2::ud.are.convertible, logical(1), u2 = tounit)]

  convList <- lapply(to, whichCanConvert) # Which are able to be converted to what

  for (i in 1:length(convList)) {
    if (length(convList[[i]]) == 0)
      next
    inds <- from %in% convList[[i]]
    out[inds, ]$x = conv2(x[inds], from[inds], to[i])
    out[inds, ]$units <- to[i]
  }

  if (inconvertibles == "omit")
    out <- out[out$units %in% to, ]

  out
}

toSI <- function(x, units) {

  rows <- match(x = tolower(units), table = tolower(unitTable$unit))
  multby <- unitTable$multby[rows]
  out <- x * multby

  out
}

fromSI <- function(x, units) {
  rows <- match(x = tolower(units), table = tolower(unitTable$unit))
  divby <- unitTable$multby[rows]
  out <- x / divby

  out
}

convertUnits_r <- function(x, from, to, inconvertibles = c("preserve", "omit")) {

  inconvertibles <- match.arg(inconvertibles)

  noneed <- from == to
  need <- !noneed

  out <- data.frame(x = x, units = from, stringsAsFactors = FALSE)

  x_si <- toSI(x[need], units = from[need])
  x_conv <- fromSI(x_si, units = to[need])

  nas <- is.na(x_conv)

  out$x[need][!nas] <- x_conv[!nas]
  out$units[need][!nas] <- to[need][!nas]

  if (inconvertibles == "omit") {
    out[need,][nas] <- NA
    out <- na.omit(out)
  }

  out$units[is.na(out$x)] = to[is.na(out$x)] # to be consistent with udunits behavior

  out
}
