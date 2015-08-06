context("model validation")

test_that("crossvalidation works for rcgams", {

  data(Phosphorus)
  foo = makeModelData(Phosphorus)
  mod2 = rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), foo)

  expect_less_than(crossvalidate(mod2), 1)
  expect_less_than(crossvalidate(mod2, kfolds = 5), 1)
})

