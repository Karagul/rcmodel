context("rcgam calibration")

test_that("rcgam calibration works for data frames and rcData", {
  data(Phosphorus)
  pdat = makeModelData(Phosphorus)

  expect_is(rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), pdat), "rcgam")
  expect_is(rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), Phosphorus), "rcgam")

  nocall <- function(obj) {obj$call = NULL; obj}
  expect_equal(nocall(rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), pdat)),
                   nocall(rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), Phosphorus)))
})

# test_that("ggTermPlot function works with model objects", {
#   data(Phosphorus)
#   pdat = makeModelData(Phosphorus)
#   mod1 = rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), pdat)
#   expect_is("gg", )
# })

test_that("rcgams can be converted to gams", {
  data(Phosphorus)
  library(mgcv)
  pdat = makeModelData(Phosphorus)
  mod2 <- rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), pdat)

  expect_is(as.gam(mod2), "gam")
  expect_false(is(as.gam(mod2), "rcgam"))

  expect_identical(predict.gam(as.gam(mod2)), predict(as.gam(mod2)))

  data(goodgam, package = "markstats")
  expect_identical(attributes(as.gam(mod2)), attributes(goodgam))
})
