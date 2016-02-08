context("synthtic data generation")


test_that("synthetic data is useable by rcmodel functions", {

  synth1 = makeSyntheticCohn(nrows = 20, sigsq = 0.05, log_detlim = 0)

  gam1 = rcgam(formula = c ~ q, data = synth1)
  expect_is(gam1, "rcgam")
  # expect_equal(mean(predict(gam1)), mean(synth1$conc))
})
