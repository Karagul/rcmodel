context("rclm prediction")

test_that("predict.rclm returns expected values and exceptions", {
  data(Phosphorus)
  pdat = makeModelData(Phosphorus)
  mod2 = rclm(c ~ q + sharm(Date) + time, pdat)

  expect_is(predict(mod2), "list")
  expect_is(predict(mod2, newdata = Phosphorus[1:10, ]), "list")
  expect_is(predict(mod2, se.fit = TRUE), "list")

  expect_equal(length(predict(mod2, se.fit = TRUE)), 2)
  expect_equal(length(predict(mod2)), 1)

  expect_warning(predict(mod2, newdata = Phosphorus[1:10, ], smear = TRUE, retrans = FALSE))
})

test_that("rclm can predict using raw data and subsets thereof", {
  data(Phosphorus)
  pdat = makeModelData(Phosphorus)
  mod2 = rclm(c ~ q + sharm(Date) + time, pdat)

  expect_equal(predict(mod2, newdata = Phosphorus), predict(mod2))
  expect_equal(predict(mod2, newdata = Phosphorus[1:10, ])$fit, predict(mod2)$fit[1:10])
})

test_that("predictions are unbiased", {
  data(Phosphorus)
mod2 = rclm(c ~ q + sharm(Date) + time, Phosphorus)
  expect_lt(abs(mean(predict(mod2, smear = TRUE)$fit) - mean(Phosphorus$conc)),
                   .Machine$double.eps)
  expect_gt(abs(mean(predict(mod2, smear = FALSE)$fit) - mean(Phosphorus$conc)),
                   .Machine$double.eps)

  expect_lt(abs(mean(predict(mod2, retrans = FALSE, smear = FALSE)$fit)),
                   .Machine$double.eps)
})

test_that("untransformed predictions are same as lm predictions", {
  data(Phosphorus)
  pdat = makeModelData(Phosphorus)
  mod2 = rclm(c ~ q + sharm(Date) + time, pdat)

  expect_equal(predict(mod2, smear = FALSE, retrans = FALSE)$fit,
               stats::predict.lm(mod2))
})

test_that("conditional samples are correct", {
  data("rc_synth")
  mod1 = rclm(c ~ q + sharm(Date) + time, rc_synth)
  q10 <- markstats::condlSample(mod1, quantile = 0.1)
  q90 <- markstats::condlSample(mod1, quantile = 0.9)

  expect_lt(mean(q10), mean(rc_synth$conc))
  expect_gt(mean(q90), mean(rc_synth$conc))

  expect_lt(sum(q10 > rc_synth$conc), 30)
  expect_gt(sum(q10 > rc_synth$conc), 19)
  expect_lt(sum(q90 < rc_synth$conc), 30)
  expect_gt(sum(q90 < rc_synth$conc), 19)

  rc_synth
})

test_that("restricting predictions works", {
  data(Phosphorus)
  pdat = makeModelData(Phosphorus)
  mod2 = rclm(c ~ q + sharm(Date) + time, pdat)

  hinewdat = makeRawData(data.frame(q = 10, c = 0, Date = mean(Phosphorus$Date),
                                    time = 0, is.bdl = FALSE), rcmodel = mod2)
  lonewdat = makeRawData(data.frame(q = -10, c = 0, Date = mean(Phosphorus$Date),
                                    time = 0, is.bdl = FALSE), rcmodel = mod2)

  expect_equal(as.numeric(predict(mod2, restrict = TRUE, smear = FALSE,
                            retrans = FALSE, newdata = hinewdat)$fit),
                    max(mod2$model$c))

  expect_equal(as.numeric(predict(mod2, restrict = TRUE, smear = FALSE,
                                  retrans = FALSE, newdata = lonewdat)$fit),
               min(mod2$model$c))
})
