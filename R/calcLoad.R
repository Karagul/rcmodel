#' Calculate loads from concentration and flow
#'
#' Performs appropriate unit conversion to go from flow and concentration to load
#'
#' @param flow stream discharge (volume per timie)
#' @param conc constituent concentration (mass per volume)
#' @param datetime Time of observations, as a Date or POSIXct object.
#' @param flow.units options are "ft3/s" (default), "m3/s", and "l/s"
#' @param conc.units options are "mg/l" (default), "ug/l", and "ng/l"
#' @param load.units options are "kg/day"
#' @return data.frame with columns "datetime" and "load", also attribute "load.units"
#' @importFrom assertthat assert_that
#' @export

loadTS <- function(flow, conc, datetime,
                     flow.units = "ft3/s", conc.units = "mg/l", load.units = "kg/day") {
  flow.units = validateUnits(flow.units)
  conc.units = validateUnits(conc.units)
  load.units = validateUnits(load.units)

  assert_that(all(flow >= 0) && all(conc >= 0),
              length(flow) == length(conc) && length(conc) == length(datetime),
              is.time(datetime) || is.date(datetime))

  flow_si <- convertUnits(flow, from = flow.units, to = "m3/s")[["x"]]
  conc_si <- convertUnits(conc, from = conc.units, to = "kg/m3")[["x"]]

  load_si <- flow_si * conc_si # kg/s
  load <- convertUnits(load_si, from = "kg/s", to = load.units)[["x"]]

  out <- structure(data.frame(datetime = datetime,
                    load = load, load.units = load.units),
                   row.names = seq_along(load),
                   load.units = load.units)
  out
}

#' Calculate total load from a time series of estimated loads
#' Uses trapezoid approximation for integral.
#' @param load time series of loads.
#' @importFrom assertthat assert_that is.date is.time
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

