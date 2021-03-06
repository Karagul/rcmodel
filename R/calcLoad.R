#' Calculate loads from concentration and flow
#'
#' Performs appropriate unit conversion to go from flow and concentration to load
#'
#' @param flow stream discharge (volume per time)
#' @param conc constituent concentration (mass per volume)
#' @param datetime Time of observations, as a Date or POSIXct object.
#' @param flow.units options are "ft3/s" (default, also recognizes "CFS"), "m3/s", and "l/s"
#' @param conc.units options are "mg/l" (default), "ug/l", and "ng/l"
#' @param load.units options are "kg/day"
#' @return data.frame with columns "datetime" and "load", also attribute "load.units"
#' @importFrom assertthat assert_that
#' @export

loadTS <- function(flow, conc, datetime,
                     flow.units = "ft3/s", conc.units = "mg/l", load.units = "kg/day") {

  assert_that(length(conc) == length(datetime),
  is.time(datetime) || is.date(datetime))

  load <- calcLoad(flow = flow, conc = conc, flow.units = flow.units,
           conc.units = conc.units, load.units = load.units)

  out <- structure(data.frame(datetime = datetime,
                    load = load, load.units = load.units),
                   row.names = seq_along(load),
                   load.units = load.units)
  out
}

#' Calculate total load from a time series of estimated loads
#'
#' Uses trapezoid approximation for integral.
#' @param load time series of loads.
#' @param datetime dates/times corresponding to loads. Can be either a
#' "time" or "Date" object.
#' @param load.units units for the "load" time-series input. Can either be
#' "kg/day" (default), "kg/yr", or "tonne/yr".
#' @importFrom assertthat assert_that is.date is.time
#' @return A single number--the mass load in whatever units are in the numerator of
#' load.units (defult is kg/day, meaning the result is in units of kg)
#' @export
totLoad <- function(load, datetime,
                    load.units = c("kg/day", "kg/yr", "tonne/yr")) {
  load.units <- match.arg(load.units)
  assert_that(is.date(datetime) || is.time(datetime),
              length(load) == length(datetime))
  sitimeconst <- c("kg/day" = 1 / (24 * 3600),
                   "kg/yr" = 1 / (24 * 3600 * 365.25),
                   "tonne/yr" = 1 / (24 * 3600 * 365.25)) # This relative to seconds
  load_persec <- load * sitimeconst[load.units]
  n <- length(datetime)

  dt <- difftime(datetime[2:n], datetime[1:(n - 1)], units = "sec")
  out <- sum(as.numeric(dt) * 0.5 * (load_persec[2:n] + load_persec[1:(n - 1)]))
  out
}


#' Calculate mass flux from concentration and flow
#'
#' Performs appropriate unit conversion to go from flow and concentration to load
#'
#' @param flow stream discharge (volume per time)
#' @param conc constituent concentration (mass per volume)
#' @param flow.units options are "ft3/s" (default), "m3/s", and "l/s"
#' @param conc.units options are "mg/l" (default), "ug/l", and "ng/l"
#' @param load.units options are "kg/day"
#' @return vector of fluxes in specified units
#' @importFrom assertthat assert_that
#' @export
calcLoad <- function(flow, conc, flow.units = "ft3/s", conc.units = "mg/l",
                     load.units = "kg/day") {
  flow.units = validateUnits(flow.units)
  conc.units = validateUnits(conc.units)
  load.units = validateUnits(load.units)

  assert_that(all(flow >= 0) && all(conc >= 0),
              length(flow) == length(conc))

  flow_si <- convertUnits(flow, from = flow.units, to = "m3/s")[["x"]]
  conc_si <- convertUnits(conc, from = conc.units, to = "kg/m3")[["x"]]

  load_si <- flow_si * conc_si # kg/s
  load <- convertUnits(load_si, from = "kg/s", to = load.units)[["x"]]

  load
}
