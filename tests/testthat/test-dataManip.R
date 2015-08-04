# test for good behavior of data manipulation functions

context("data manipulation")

test_that("makeModelData and makeRawData are inversions of each other", {
  data(sampleData)
  foo = makeModelData(sampleData)
  bar = makeModelData(makeRawData(foo))
  attributes(foo) = NULL
  attributes(bar) = NULL
  expect_identical(bar, foo)
})
