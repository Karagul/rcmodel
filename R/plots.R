# plots.R
# Plotting functions for rcmodel objects

#' @export
ggTermPlot.rcgam <- function(object, xvar, data = NULL, ...) {
  obj <- as.gam(object)
  out <- ggTermPlot(obj, xvar, data, ...)
  out
}
