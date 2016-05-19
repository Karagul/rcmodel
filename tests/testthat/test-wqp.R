context("wqp data retrievals and checks")

test_that("unit conversion works as expected", {

  expect_error(convertUnits(1, c("abc", "def"), "ghi"))
  expect_equal(convertUnits(1, "mg/L", "ug/L"),
               data.frame(x = 1000, units = "ug/L", stringsAsFactors = FALSE))

  expect_equal(convertUnits(1, "abc", "abc"),
               data.frame(x = 1, units = "abc", stringsAsFactors = FALSE))

  expect_equal(convertUnits(c(1, 1), c("mg/L", "def"), to = c("def")),
               data.frame(x = c(1, 1), units = c("mg/L", "def"),
                          stringsAsFactors = FALSE))

  t1 <- convertUnits(c(1, 1), c("mg/L", "def"), to = c("def"),
               inconvertibles = "omit")
  t2 <- data.frame(x = c(1), units = c("def"),
                          stringsAsFactors = FALSE)
  expect_equal(t1[[1]], t2[[1]])
  expect_equal(t1[[2]], t2[[2]])

  expect_equal(convertUnits(c(1, 1), c("mg/L", "def"), to = c("ug/L")),
               data.frame(x = c(1000, 1), units = c("ug/L", "def"),
                          stringsAsFactors = FALSE))

  expect_equal(convertUnits(c(1, 1), c("mg/L", "def"), to = c("ug/L", "def")),
               data.frame(x = c(1000, 1), units = c("ug/L", "def"),
                          stringsAsFactors = FALSE))

  expect_equal(convertUnits(c(1, 1), c("mg/L", "mg/kg"), to = c("ug/L", "ug/kg")),
               data.frame(x = c(1000, 1000), units = c("ug/L", "ug/kg"),
                          stringsAsFactors = FALSE))

  expect_equal(convertUnits(c(NA, NA), c("mg/l", "mg/kg"), to = c("ug/L", "ug/kg")),
               data.frame(x = c(NA_real_, NA_real_), units = c("ug/L", "ug/kg"),
                          stringsAsFactors = FALSE))
})


test_that("wqp unit check works", {
  data(aluminumData)
  testdata <- aluminumData
  convto <- c("mg/l", "mg/kg")
  checked <- wqp_checkUnits(testdata, convertTo = convto)

  units0 <- testdata$ResultMeasure.MeasureUnitCode
  vals0 <- testdata$ResultMeasureValue
  units1 <- checked$ResultMeasure.MeasureUnitCode
  vals1 <- checked$ResultMeasureValue

  dlunits0 <- testdata$DetectionQuantitationLimitMeasure.MeasureUnitCode
  dlvals0 <- testdata$DetectionQuantitationLimitMeasure.MeasureValue
  dlunits1 <- checked$DetectionQuantitationLimitMeasure.MeasureUnitCode
  dlvals1 <- checked$DetectionQuantitationLimitMeasure.MeasureValue

  expect_true(all(units1 %in% convto))
  expect_true(all(dlunits1 %in% convto))

  sum(is.na(vals1))
  sum(is.na(vals0))

  units0[is.na(vals1) & !is.na(vals0)]

  # units in checked must be one of convto
  # values in checked may be coerced to NA if unit conversion failed
  # units in testdata may be NA if not reported for zero values

  # expect zero-value units to be converted
  expect_gt(sum(vals0 == 0 & is.na(units0), na.rm = TRUE),
            sum(vals1 == 0, na.rm = TRUE))
  # expect rows with pcodes to all be converted

})


test_that("wqp fraction check works", {
  data("aluminumData")
  testdata <- aluminumData

  expect_message(wqp_checkFraction(testdata))
  expect_silent(wqp_checkFraction(testdata, silent = TRUE))
  expect_lt(nrow(wqp_checkFraction(testdata)),
            nrow(testdata))
})

test_that("wqp detection limit check works", {
  data("aluminumData")
  testdata <- aluminumData
  checked <- wqp_checkBDL(testdata)

  expect_null(testdata$is.bdl)
  expect_is(checked$is.bdl, "logical")

  # test unit conversion
  narows <- is.na(checked$ResultMeasure.MeasureUnitCode) |
    is.na(checked$DetectionQuantitationLimitMeasure.MeasureUnitCode)
  expect_true(all(checked$ResultMeasure.MeasureUnitCode[!narows] ==
               checked$DetectionQuantitationLimitMeasure.MeasureUnitCode[!narows]))



})



