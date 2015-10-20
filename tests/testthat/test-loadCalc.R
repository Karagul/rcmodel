context("Load calculation")


test_that("calcLoad flags improper inputs", {
  goodconc <- runif(10)
  goodflow <- runif(10)
  badconc <- runif(10, -1, 0)
  badflow <- runif(10, -1, 0)

  expect_error(calcLoad(goodflow, badconc))
  expect_error(calcLoad(badflow, goodconc))
  expect_is(calcLoad(goodflow, goodconc), "numeric")
})


test_that("calcLoad unit conversion works as expected", {
  cfs = 22
  mgl = 0.5

  expect_equal(calcLoad(cfs, mgl), calcLoad(cfs, mgl * 1000, conc.units = "ug/l"))
  expect_equal(calcLoad(cfs, mgl), calcLoad(cfs, mgl * 1E06, conc.units = "ng/l"))
})
