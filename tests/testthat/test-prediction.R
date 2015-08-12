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
