# plots.R
# Plotting functions for rcmodel objects

#' @importFrom markstats ggTermPlot
#'
ggTermPlot.rcgam <- function(object, xvar, data = NULL, ...) {
  obj <- as.gam(object)
  out <- ggTermPlot(obj, xvar, data, ...)
  out
}
