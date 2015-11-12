context("model validation")


test_that("differential split-sample tests work as intended", {
  data("rc_synth")
  mod1 = rcgam(c ~ s(q, k = 5) + s(doy, bs = "cc", k = 4) + s(time), rc_synth)
  qtl <- 0.9
  maxlq <- quantile(scale(log(markstats::getData(mod1)$flow)), qtl)

  conc1 <- splitSampleTest(mod1, scale(log(flow)) > maxlq)
  load1 <- splitSampleTest(mod1, scale(log(flow)) > maxlq, what = "load")
  conc2 <- splitSampleTest(mod1, scale(log(flow)) > maxlq,
                           retransform = FALSE)

  expect_error(splitSampleTest(mod1, scale(log(flow)) > maxlq, what = "load",
                               retransform = FALSE))

  expect_is(conc1, "numeric")
  expect_is(conc2, "numeric")
  expect_is(load1, "numeric")


  expect_equal(length(splitSampleTest(mod1, flow > max(flow))), 0)
  expect_is(splitSampleTest(mod1, flow > max(flow), incl.data = TRUE),
            "list")
  expect_less_than(nrow(splitSampleTest(mod1, q > 1, incl.data = TRUE)$data),
                   nrow(splitSampleTest(mod1, q > 0.5, incl.data = TRUE)$data))
  testErrs <- splitSampleTest(mod1, scale(log(flow)) > maxlq)
  expect_more_than((length(testErrs) + 1) / nrow(rc_synth), 1 - qtl)
  expect_less_than((length(testErrs) - 1) / nrow(rc_synth), 1 - qtl)
})


test_that("scaling of validation residuals works", {
  data("rc_synth")
  mod1 = rcgam(c ~ s(q, k = 5) + s(doy, bs = "cc", k = 4) + s(time), rc_synth)
  qtl <- 0.9
  maxlq <- quantile(scale(log(markstats::getData(mod1)$flow)), qtl)

  expect_error(splitSampleTest(mod1, scale(log(flow)) > maxlq, scale = "gcv",
                               retransform = TRUE))

  # conc, log space
  expect_less_than(mean(abs(splitSampleTest(mod1, scale(log(flow)) > maxlq,
                                            scale = "gcv",
                                            retransform = FALSE))),
                   10)
  expect_more_than(mean(abs(splitSampleTest(mod1, scale(log(flow)) > maxlq,
                                            scale = "gcv",
                                            retransform = FALSE))),
                   0.5)

  # load
  expect_less_than(mean(abs(splitSampleTest(mod1, scale(log(flow)) > maxlq,
                                            what = "load",
                                            scale = "cv"))),
                   10)
  expect_more_than(mean(abs(splitSampleTest(mod1, scale(log(flow)) > maxlq,
                                            scale = "cv",
                                            what = "load"))),
                   0.5)

  # concentration, retransformed
  expect_less_than(mean(abs(splitSampleTest(mod1, scale(log(flow)) > maxlq,
                                            scale = "cv",
                                            retransform = TRUE))),
                   10)
  expect_more_than(mean(abs(splitSampleTest(mod1, scale(log(flow)) > maxlq,
                                            scale = "cv",
                                            retransform = TRUE))),
                   0.5)
})


