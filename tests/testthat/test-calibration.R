context("model calibration")

test_that("rcgam calibration works for data frames and rcData", {
  data(Phosphorus)
  pdat = makeModelData(Phosphorus)

  expect_is(rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), pdat), "rcgam")
  expect_is(rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), Phosphorus), "rcgam")

  nocall <- function(obj) {obj$call = NULL; obj}
  expect_equal(nocall(rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), pdat)),
                   nocall(rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), Phosphorus)))
})

test_that("rclm calibration works for data frames and rcData", {
  data(Phosphorus)
  pdat = makeModelData(Phosphorus)

  expect_is(rclm(c ~ q + sharm(Date) + time, pdat), "rclm")
  expect_is(rclm(c ~ q + sharm(Date) + time, Phosphorus), "rclm")

  nocall <- function(obj) {obj$call = NULL; obj}
  expect_equal(nocall(rclm(c ~ q + sharm(Date) + time, pdat)),
               nocall(rclm(c ~ q + sharm(Date) + time, Phosphorus)))
})


