context("conditional samples from model output")

test_that("condlSample works for rcgam objects", {
  data(Phosphorus, package = "rcmodel")
  data(rc_synth, package = "rcmodel")
  library(dplyr)

  mod1 = rcmodel::rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time),
                        rc_synth)

  mod2 = rcmodel::rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time),
                        Phosphorus)
  newdata2 = data.frame(Date = as.Date("1986-09-17"), conc = 0.1,
                        conc.units = "mg/l", flow = 10,
                        flow.units = "CFS", is.bdl = FALSE)

  expect_is(condlSample(mod2, newdata = newdata2, quantile = 0.9),
            "numeric")
  expect_is(condlSample(mod2, quantile = 0.9),
            "numeric")
})

test_that("conditional samples are correct for rcgams", {
  data("rc_synth", package = "rcmodel")
  data("Phosphorus", package = "rcmodel")
  mod1 = rcmodel::rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time),
                        rc_synth)
  mod2 = rcmodel::rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time),
                        Phosphorus)
  n <- nrow(Phosphorus)

  q10 <- condlSample(mod2, quantile = 0.1)
  q90 <- condlSample(mod2, quantile = 0.9)

  expect_less_than(mean(q10), mean(Phosphorus$conc))
  expect_more_than(mean(q90), mean(Phosphorus$conc))

  expect_less_than(sum(q10 > Phosphorus$conc), n / 5)
  expect_more_than(sum(q10 > Phosphorus$conc), n / 20)
  expect_less_than(sum(q90 < Phosphorus$conc), n / 5)
  expect_more_than(sum(q90 < Phosphorus$conc), n / 20)
})
