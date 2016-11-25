context("Load calculation")


test_that("loadTS returns as expected", {
  n <- 10
  dt <- seq.Date(Sys.Date() - n, length.out = n, by = 1)
  goodconc <- runif(n)
  goodflow <- runif(n)
  badconc <- runif(n, -1, 0)
  badflow <- runif(n, -1, 0)

  expect_error(loadTS(goodflow, badconc, dt))
  expect_error(loadTS(badflow, goodconc, dt))
  expect_is(loadTS(goodflow, goodconc, dt), "data.frame")
  expect_equal(nrow(loadTS(goodflow, goodconc, dt)), n)
})


test_that("loadTS unit conversion works as expected", {
  cfs = 22
  mgl = 0.5
  dt = Sys.Date()

  expect_equal(loadTS(cfs, mgl, dt),
               loadTS(cfs, mgl * 1000, conc.units = "ug/l", dt))
  expect_equal(loadTS(cfs, mgl, dt),
               loadTS(cfs, mgl * 1E06, conc.units = "ng/l", dt))
})

