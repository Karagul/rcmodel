#' Calculate loads from concentration and flow
#'
#' Performs appropriate unit conversion to go from flow and concentration to load
#'
#' @param flow stream discharge (volume per timie)
#' @param conc constituent concentration (mass per volume)
#' @param flow.units options are "CFS" (default), "m3/s", and "l/s"
#' @param conc.units options are "mg/l" (default), "ug/l", and "ng/l"
#' @param load.units options are "kg/day"
#'
#' @export

calcLoad <- function(flow, conc,
                     flow.units = c("CFS", "m3/s", "l/s"),
                     conc.units = c("mg/l", "ug/l", "ng/l"),
                     load.units = c("kg/day", "kg/yr", "tonne/yr")) {
  flow.units = match.arg(flow.units)
  conc.units = match.arg(conc.units)
  load.units = match.arg(load.units)

  stopifnot(all(flow >= 0) && all(conc >= 0))
  siflowconst <- c("CFS" = 28.3168466, "m3/s" = 1)
  siconcconst <- c("mg/l" = 1E-06, "ug/l" = 1E-09, "ng/l" = 1E-12)
  siloadconst <- c("kg/day" = 1 / (24 * 3600))

  out = flow * siflowconst[flow.units] * conc * siconcconst[conc.units] / siloadconst[load.units]
  unname(out)
}
