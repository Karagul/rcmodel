context("rcgam prediction")

test_that("predict.rcgam returns expected values and exceptions", {
  data(Phosphorus)
  pdat = makeModelData(Phosphorus)
  mod2 = rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), pdat)

  expect_is(predict(mod2), "list")
  expect_is(predict(mod2, newdata = Phosphorus[1:10, ]), "list")
  expect_is(predict(mod2, se.fit = TRUE), "list")

  expect_equal(length(predict(mod2, se.fit = TRUE)), 2)
  expect_equal(length(predict(mod2)), 1)

  expect_warning(predict(mod2, newdata = Phosphorus[1:10, ], smear = TRUE, retrans = FALSE))
})

test_that("rcgam can predict using raw data and subsets thereof", {
  data(Phosphorus)
  pdat = makeModelData(Phosphorus)
  mod2 = rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), pdat)

  expect_equal(predict(mod2, newdata = Phosphorus), predict(mod2))
  expect_equal(predict(mod2, newdata = Phosphorus[1:10, ])$fit, predict(mod2)$fit[1:10])
})

test_that("predictions are unbiased", {
  data(Phosphorus)
  mod2 = rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), Phosphorus)
  expect_lt(abs(mean(predict(mod2, smear = TRUE)$fit) - mean(Phosphorus$conc)),
                   .Machine$double.eps * 10)
  expect_gt(abs(mean(predict(mod2, smear = FALSE)$fit) - mean(Phosphorus$conc)),
                   .Machine$double.eps)

  expect_lt(abs(mean(predict(mod2, retrans = FALSE, smear = FALSE)$fit)),
                   .Machine$double.eps * 10)
})

test_that("untransformed predictions are same as gam predictions", {
  data(Phosphorus)
  pdat = makeModelData(Phosphorus)
  mod2 = rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), pdat)

  expect_equal(predict(mod2, smear = FALSE, retrans = FALSE)$fit,
               mgcv::predict.gam(mod2))
})

test_that("restricting predictions works", {
  data(Phosphorus)
  pdat = makeModelData(Phosphorus)
  mod2 = rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), pdat)

  hinewdat = makeRawData(data.frame(q = 10, c = 0, doy = 10,
                                    time = 0, is.bdl = FALSE), rcmodel = mod2)
  lonewdat = makeRawData(data.frame(q = -10, c = 0, doy = 10,
                                    time = 0, is.bdl = FALSE), rcmodel = mod2)

  expect_equal(as.numeric(predict(mod2, restrict = TRUE, smear = FALSE,
                            retrans = FALSE, newdata = hinewdat)$fit),
                    max(mod2$model$c))

  expect_equal(as.numeric(predict(mod2, restrict = TRUE, smear = FALSE,
                                  retrans = FALSE, newdata = lonewdat)$fit),
               min(mod2$model$c))
})

test_that("prediction of terms works", {
  data(Phosphorus)
  pdat = makeModelData(Phosphorus)
  mod2 = rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), pdat)
  expect_warning(predict(mod2, type = "terms"))
  expect_is(predict(mod2, smear = FALSE, retransform = FALSE, restrict = FALSE, type = "terms"), "matrix")
})
