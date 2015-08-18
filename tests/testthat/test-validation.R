context("model validation")

test_that("crossvalidation works for rcgams", {

  data(Phosphorus)
  foo = makeModelData(Phosphorus)
  mod2 = rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), foo)

  conccvs = c(
    concloo = crossvalidate(mod2, what = "conc"),
    conc5f = crossvalidate(mod2, what = "conc", kf = 5),
    concloo_nosmear = crossvalidate(mod2, what = "conc", smear = FALSE),
    conc5f_nosmear = crossvalidate(mod2, what = "conc", smear = FALSE, kf = 5)
  )

  loadcvs = c(
    loadloo = crossvalidate(mod2, what = "load"),
    load5f = crossvalidate(mod2, what = "load", kf = 5),
    loadloo_nosmear = crossvalidate(mod2, what = "load", smear = FALSE),
    load5f_nosmear = crossvalidate(mod2, what = "load", smear = FALSE, kf = 5)
  )

  expect_is(conccvs, "numeric")
  expect_is(crossvalidate(mod2, kfolds = 5), "numeric")

  expect_true(all(conccvs < 1))
  expect_true(all(loadcvs < 1))

})

test_that("crossvalidation works for log-space estimates", {
  data(Phosphorus)
  foo = makeModelData(Phosphorus)
  mod2 = rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), foo)

  concloo = crossvalidate(mod2, what = "conc", retransform = FALSE)

  conc5f = crossvalidate(mod2, what = "conc", retransform = FALSE, kf = 5)
  concloo_nosmear = crossvalidate(mod2, what = "conc", retransform = FALSE,
                                 smear = FALSE)

  expect_is(concloo, "numeric")
  expect_is(conc5f, "numeric")

  expect_less_than(concloo, 1)
  expect_less_than(conc5f, 1)
  expect_equal(concloo, concloo_nosmear)

  expect_error(crossvalidate(mod2, what = "load", retransform = FALSE))

#   mod2$gcv.ubre
#   crossvalidate(mod2, retransform = FALSE, statistic = "mse")
})


test_that("differential split-sample tests work as intended", {
  data("rc_synth")
  mod1 = rcgam(c ~ s(q, k = 5) + s(doy, bs = "cc", k = 4) + s(time), rc_synth)

  qtl <- 0.9
  maxlq <- quantile(scale(log(getData(mod1)$flow)), qtl)

  testErrs <- splitSampleTest(mod1, scale(log(flow)) > maxlq)
#   hist(testErrs)

  expect_is(testErrs, "numeric")
  expect_more_than((length(testErrs) + 1) / nrow(rc_synth), 1 - qtl)
  expect_less_than((length(testErrs) - 1) / nrow(rc_synth), 1 - qtl)

})


