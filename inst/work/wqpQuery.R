



boundWQ$ResultMeasure.MeasureUnitCode %>%
  trimws() %>%
  as.factor() %>%
  summary()


boundWQ %>%
  group_by(ResultMeasure.MeasureUnitCode, ResultSampleFractionText) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  print(n = 200)

# checkin suspicious cases

# When fraction is blank: "", "%", "pg/g", "count", "ug", "code"

boundWQ %>%
  filter(ResultSampleFractionText == "",
         ResultMeasure.MeasureUnitCode == "") %>%
  sample_n(10) %>%
  glimpse()


boundWQ %>%
  mutate(ResultMeasure.MeasureUnitCode = trimws(ResultMeasure.MeasureUnitCode)) %>%
  filter(ResultSampleFractionText == "",
         ResultMeasure.MeasureUnitCode == "ug") %>%
  `[[`("CharacteristicName") %>%
  as.factor() %>%
  summary()

# What about blank units?
boundWQ %>%
  filter(ResultMeasure.MeasureUnitCode == "") %>%
  sample_n(10) %>%
  glimpse()

# Possible ways to get units:
# 1. USGS pcode
# 2. detection limit units
# 3. Units of other observations in dataset

# Try number 3, but would only use if 1 doesn't work.
testrow <- boundWQ %>%
  filter(ResultMeasure.MeasureUnitCode == "",
         USGSPCode == "",
         !is.na(ResultMeasureValue),
         ResultMeasureValue > 0) %>%
  sample_n(1)
glimpse(testrow)
testdf <- boundWQ %>%
  filter(CharacteristicName == testrow$CharacteristicName,
         MonitoringLocationIdentifier == testrow$MonitoringLocationIdentifier,
         ResultSampleFractionText == testrow$ResultSampleFractionText)
glimpse(testdf)
testdf$ResultMeasure.MeasureUnitCode %>%
  as.factor() %>%
  summary()

# Looks good, but with the occasional "mg/kg" mixed in with "mg/l". From the following,
# I see that the mg/kg is dry sediment, not concentration.

# testdf %>%
#   filter(ResultMeasure.MeasureUnitCode != "") %>%
#   glimpse()

# Look at distribution by unit
library(ggplot2)
testdf %>%
  dplyr::filter(trimws(ResultMeasure.MeasureUnitCode) %in% c("mg/l", "")) %>%
  ggplot(aes(x = ResultMeasure.MeasureUnitCode,
                          y = ResultMeasureValue)) +
  geom_boxplot()


# Bottom line: I can recover unit information from USGS pcode, and if not from
# that then it's probably zero anyway. If it's not zero, then assume it's the
# same unit as reported elsewhere in the dataset.


