context("unit conversion")

test_that("unit conversion works as expected", {

  expect_error(convertUnits(1, c("abc", "def"), "ghi"))

  expect_equal(convertUnits(1, "mg/L", "ug/L", use.ud = TRUE),
               data.frame(x = 1000, units = "ug/L", stringsAsFactors = FALSE))
  expect_equal(convertUnits(1, "mg/L", "ug/L", use.ud = FALSE),
               data.frame(x = 1000, units = "ug/L", stringsAsFactors = FALSE))


  expect_equal(convertUnits(1, "abc", "abc", use.ud = TRUE),
               data.frame(x = 1, units = "abc", stringsAsFactors = FALSE))
  expect_equal(convertUnits(1, "abc", "abc", use.ud = FALSE),
               data.frame(x = 1, units = "abc", stringsAsFactors = FALSE))

  # Don't do a conversion when one cannot be done
  expect_equal(convertUnits(c(1, 1), c("mg/L", "def"), to = c("def"), use.ud = TRUE),
               data.frame(x = c(1, 1), units = c("mg/L", "def"),
                          stringsAsFactors = FALSE))
  expect_equal(convertUnits(c(1, 1), c("mg/L", "def"), to = c("def"), use.ud = FALSE),
               data.frame(x = c(1, 1), units = c("mg/L", "def"),
                          stringsAsFactors = FALSE))

  # omitting inconvertibles
  t1.1 <- convertUnits(c(1, 1), c("mg/L", "def"), to = c("def"),
                     inconvertibles = "omit", use.ud = TRUE)
  t1.2 <- convertUnits(c(1, 1), c("mg/L", "def"), to = c("def"),
                       inconvertibles = "omit", use.ud = FALSE)
  t2 <- data.frame(x = c(1), units = c("def"),
                   stringsAsFactors = FALSE)
  expect_equal(t1.1[[1]], t2[[1]])
  expect_equal(t1.1[[2]], t2[[2]])
  expect_equal(t1.2[[1]], t2[[1]])
  expect_equal(t1.2[[2]], t2[[2]])


  # preserve inconvertibles
  expect_equal(convertUnits(c(1, 1), c("mg/L", "def"), to = c("ug/L"), use.ud = TRUE),
               data.frame(x = c(1000, 1), units = c("ug/L", "def"),
                          stringsAsFactors = FALSE))
  expect_equal(convertUnits(c(1, 1), c("mg/L", "def"), to = c("ug/L"), use.ud = FALSE),
               data.frame(x = c(1000, 1), units = c("ug/L", "def"),
                          stringsAsFactors = FALSE))

  expect_equal(convertUnits(c(1, 1), c("mg/L", "def"), to = c("ug/L", "def"), use.ud = TRUE),
               data.frame(x = c(1000, 1), units = c("ug/L", "def"),
                          stringsAsFactors = FALSE))
  expect_equal(convertUnits(c(1, 1), c("mg/L", "def"), to = c("ug/L", "def"), use.ud = FALSE),
               data.frame(x = c(1000, 1), units = c("ug/L", "def"),
                          stringsAsFactors = FALSE))

  expect_equal(convertUnits(c(1, 1), c("mg/L", "mg/kg"), to = c("ug/L", "ug/kg"), use.ud = TRUE),
               data.frame(x = c(1000, 1000), units = c("ug/L", "ug/kg"),
                          stringsAsFactors = FALSE))
  expect_equal(convertUnits(c(1, 1), c("mg/L", "mg/kg"), to = c("ug/L", "ug/kg"), use.ud = FALSE),
               data.frame(x = c(1000, 1000), units = c("ug/L", "ug/kg"),
                          stringsAsFactors = FALSE))

  expect_equal(convertUnits(c(NA, NA), c("mg/l", "mg/kg"), to = c("ug/L", "ug/kg"), use.ud = TRUE),
               data.frame(x = c(NA_real_, NA_real_), units = c("ug/L", "ug/kg"),
                          stringsAsFactors = FALSE))

  expect_equal(convertUnits(c(NA, NA), c("mg/l", "mg/kg"), to = c("ug/L", "ug/kg"), use.ud = FALSE),
               data.frame(x = c(NA_real_, NA_real_), units = c("ug/L", "ug/kg"),
                          stringsAsFactors = FALSE))
})
