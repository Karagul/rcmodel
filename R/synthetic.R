# This script contains functions for creating synthetic datasets for use in
# rating-curve modeling and benchmarking.
#


#' generate synthetic flow-wq dataset
#'
#' @export
#'
#'
#'

makeSyntheticData <- function(nrows = 240, seed = 156) {
  library(lubridate)

  force(seed)
  dates = as.Date(ymd("2015-01-01") - months(1:nrows))
  times = scale(as.numeric(dates), scale = 1)
  doys = as.numeric(format(dates, "%j"))

  flows = rlnorm(nrows, 5, 1.2)
  qs = scale(log(flows)) %>% as.vector()

  fc <- function(q, doy, time, plot = FALSE, randseed = seed) {
    set.seed(randseed)
    rlnorm(nrows, 5, 1.2)
    # select q relationship
    fq = function(q) q - q^2 / 2

    # select time relationship
    polycoefs = rnorm(5) / factorial(1:5) / 4
    pnl <- function(x, deg) (x /2113.175) ^ deg
    ft = function(t) sapply(1:5, pnl, x = time) %*% polycoefs


    # select doy relationship
    sincoefs = rnorm(5) / factorial(1:5) * 4
    coscoefs = rnorm(5) / factorial(1:5) * 4
    fd <- function(doy) {
      d = doy * pi / 180
      hmc <- function(deg, a, b) a * sin(d * deg) + b * cos(d * deg)
      Map(hmc, deg = 1:5, a = sincoefs, b = coscoefs) %>%
        as.data.frame() %>% unname() %>%
        apply(1, sum)
    }

    if(plot) {
      oldpar = par(no.readonly = TRUE)
      on.exit({par(oldpar)})
      par(mfrow = c(2, 2), mar = c(4, 4, 1, 3))
      plot(q, fq(q))
      plot(times, ft(times))
      plot(doys, fd(doys))
      par(oldpar)
    }

    invisible(as.vector(fq(q)) + as.vector(fd(doy)) + as.vector(ft(time)))
  }


  rc_synth = data.frame(Date = dates, flow = flows, flow.units = "ft3/s",
                        q = qs, doy = doys, time = times) %>%
    mutate(c_nonoise = fc(q, doy, time),
           noise = rnorm(nrows),
           c = c_nonoise + noise,
           conc = as.vector(exp((c - 1) * 0.2)), conc.units = "mg/L",
           is.bdl = FALSE)
  attr(rc_synth, "fcn") = fc

  rc_synth
}


#' Make a synthetic linear dataset
#' Based on description of Monte Carlo experiments in Cohn, 2005.
#' @param nrows The number of sumulations
#' @param sigsq the sigma^2 parameter. Cohn (2005) used 0.04, 0.25, 0.64, and 1.44.
#' @param log_detlim the imposed detection limit for censoring the observations.
#' Cohn (2005) used -Inf, -2*sigma, -sigma, 0, sigma
#' @return A data.frame with columns "Date" (arbitrarily simulated),
#' "conc", "flow", "conc.units", "flow.units", and "is.bdl".

makeSyntheticCohn <- function(nrows, sigsq, log_detlim = 0) {

  # Make flows
  lq <- qnorm((1:nrows - 0.63) / (nrows + 1 - 2 * 0.63))

  # Make concentrations
  lc = 0.5 * lq + rnorm(nrows, 0, sqrt(sigsq))

  # censor observations
  bdl <- lc < log_detlim
  bdl[order(lq, decreasing = TRUE)[1:3]] <- FALSE # Make sure at least 3 uncensored
  lc[bdl] <- log_detlim

  assertthat::assert_that(sum(bdl) >= 3)

  # Make returned object
  out <- data.frame(Date = seq.Date(from = as.Date("2001-01-01"),
                                    by = 30, length.out = nrows),
                    flow = exp(lq), conc = exp(lc),
                    flow.units = "ft3/s", conc.units = "mg/l", is.bdl = bdl)

  out
}

