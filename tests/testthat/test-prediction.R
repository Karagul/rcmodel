context("rcmodel prediction")


test_that("rcgam can predict using raw data and subsets thereof", {
  data(Phosphorus)
  pdat = makeModelData(Phosphorus)
  mod2 = rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), pdat)

  expect_equal(predict(mod2, newdata = Phosphorus), predict(mod2))
  expect_equal(predict(mod2, newdata = Phosphorus[1:10, ]), predict(mod2)[1:10])
})



test_that("smeared predictions are unbiased", {
  data(Phosphorus)
  mod2 = rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), Phosphorus)
  expect_less_than(abs(mean(predict(mod2, smear = TRUE)) - mean(Phosphorus$conc)),
                   .Machine$double.eps)
  expect_more_than(abs(mean(predict(mod2, smear = FALSE)) - mean(Phosphorus$conc)),
                   .Machine$double.eps)
})

test_that("untransformed predictions are same as gam predictions", {
  data(Phosphorus)
  pdat = makeModelData(Phosphorus)
  mod2 = rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), pdat)

  expect_equal(predict(mod2, retrans = FALSE), mgcv::predict.gam(mod2))
})

test_that("conditional samples are correct", {
  data("rc_synth")
  mod1 = rcgam(c ~ s(q, k = 5) + s(doy, bs = "cc", k = 4) + s(time), rc_synth)
  q10 <- condlSample(mod1, quantile = 0.1)
  q90 <- condlSample(mod1, quantile = 0.9)

  expect_less_than(mean(q10), mean(rc_synth$conc))
  expect_more_than(mean(q90), mean(rc_synth$conc))

  expect_less_than(sum(q10 > rc_synth$conc), 30)
  expect_more_than(sum(q10 > rc_synth$conc), 19)
  expect_less_than(sum(q90 < rc_synth$conc), 30)
  expect_more_than(sum(q90 < rc_synth$conc), 19)

  rc_synth
})


