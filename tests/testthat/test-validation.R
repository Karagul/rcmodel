context("model validation")


test_that("differential split-sample tests work as intended", {
  data("rc_synth")
  mod1 = rcgam(c ~ s(q, k = 5) + s(doy, bs = "cc", k = 4) + s(time), rc_synth)

  conc1 <- splitSampleTest(mod1,
                           scale(log(flow)) > quantile(scale(log(flow)), 0.9))
  load1 <- splitSampleTest(mod1,
                           scale(log(flow)) > quantile(scale(log(flow)), 0.9),
                           what = "load")
  conc2 <- splitSampleTest(mod1,
                           scale(log(flow)) > quantile(scale(log(flow)), 0.9),
                           retransform = FALSE)

  expect_error(splitSampleTest(mod1, what = "load",
     scale(log(flow)) > quantile(scale(log(flow)), 0.9),
     retransform = FALSE))

  expect_is(conc1, "numeric")
  expect_is(conc2, "numeric")
  expect_is(load1, "numeric")


  expect_equal(length(splitSampleTest(mod1, flow > max(flow))), 0)
  expect_is(splitSampleTest(mod1, flow > max(flow), incl.data = TRUE),
            "list")
  expect_lt(nrow(splitSampleTest(mod1, q > 1, incl.data = TRUE)$data),
                   nrow(splitSampleTest(mod1, q > 0.5, incl.data = TRUE)$data))
  testErrs <- splitSampleTest(mod1,
                scale(log(flow)) > quantile(scale(log(flow)), 0.9))
  expect_gt((length(testErrs) + 1) / nrow(rc_synth), 1 - 0.9)
  expect_lt((length(testErrs) - 1) / nrow(rc_synth), 1 - 0.9)
})


test_that("scaling of validation residuals works", {
  data("rc_synth")
  mod1 = rcgam(c ~ s(q, k = 5) + s(doy, bs = "cc", k = 4) + s(time), rc_synth)

  expect_error(splitSampleTest(mod1,
                   scale(log(flow)) > quantile(scale(log(flow)), 0.9), scale = "gcv",
                   retransform = TRUE))

  # conc, log space
  expect_lt(mean(abs(splitSampleTest(mod1,
                scale(log(flow)) > quantile(scale(log(flow)), 0.9), # top 10th of flows
                scale = "gcv",
                retransform = FALSE))),
                   10)
  expect_gt(mean(abs(splitSampleTest(mod1,
                scale(log(flow)) > quantile(scale(log(flow)), 0.9),
                scale = "gcv",
                retransform = FALSE))),
                   0.5)

  # load
  expect_lt(mean(abs(splitSampleTest(mod1,
            scale(log(flow)) > quantile(scale(log(flow)), 0.9),
            what = "load",
            scale = "cv"))),
                   10)
  expect_gt(mean(abs(splitSampleTest(mod1,
            scale(log(flow)) > quantile(scale(log(flow)), 0.9),
            scale = "cv",
            what = "load"))),
                   0.5)

  # concentration, retransformed
  expect_lt(mean(abs(splitSampleTest(mod1,
            scale(log(flow)) > quantile(scale(log(flow)), 0.9),
            scale = "cv",
            retransform = TRUE))),
                   10)
  expect_gt(mean(abs(splitSampleTest(mod1,
            scale(log(flow)) > quantile(scale(log(flow)), 0.9),
            scale = "cv",
            retransform = TRUE))),
                   0.5)
})


## crossvalidate.rcgam

test_that("crossvalidation works for rcgams", {

  data(Phosphorus, package = "rcmodel")
  foo = rcmodel::makeModelData(Phosphorus)
  mod2 = rcmodel::rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), foo)

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

test_that("crossvalidation works for log-space estimates in rcgams", {
  data(Phosphorus, package = "rcmodel")
  foo = rcmodel::makeModelData(Phosphorus)
  mod2 = rcmodel::rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), foo)

  concloo = crossvalidate(mod2, what = "conc", retransform = FALSE, smear = FALSE)

  conc5f = crossvalidate(mod2, what = "conc", retransform = FALSE, kf = 5)
  concloo_nosmear = crossvalidate(mod2, what = "conc", retransform = FALSE,
                                  smear = FALSE)

  expect_is(concloo, "numeric")
  expect_is(conc5f, "numeric")

  expect_lt(concloo, 1)
  expect_lt(conc5f, 1)
  expect_equal(concloo, concloo_nosmear)

  expect_error(crossvalidate(mod2, what = "load", retransform = FALSE))

  #   mod2$gcv.ubre
  #   crossvalidate(mod2, retransform = FALSE, statistic = "mse")
})
