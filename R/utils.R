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
