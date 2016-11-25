# wqData.R
# Functions defining wqData objects and operations thereupon


# Getting data ------------------------------------------------------------

#' Retrieve data.frame of all characteristic names available from WQP
#' @export
getWQPCodes <- function(name = c("statecode", "countycode", "Sitetype",
                                 "Organization", "sampleMedia",
                                 "Characteristictype", "Characteristicname",
                                 "providers")) {
  name <- match.arg(name)
  charListURL <- paste0("http://www.waterqualitydata.us/Codes/",
                        name,
                        "?mimeType=json")
  out <- jsonlite::fromJSON(charListURL)[["codes"]]
  out
}

#' Lookup characteristic names available from WQP
#' @param pattern text string to match
#' @param ... other arguments passed to grepl
charNameLookup <- function(pattern, ignore.case = TRUE, ...) {
  names <- getWQPCodes("Characteristicname")
  out <- names[grepl(pattern = pattern, x = names$value,
                     ignore.case = ignore.case, ...), ]
  out
}

#' Get and show bounding box
bbox <- function(west, south, east, north, plot = TRUE) {
  out <- paste(c(west, south, east, north), collapse = ",")
  if(plot) {
    l1 <- leaflet() %>%
      addTiles() %>%
      addRectangles(lng1 = west, lat1 = north,
                    lng2 = east, lat2 = south)
    print(l1)
  }
  out
}

#' Improvements on dataRetrieval functions.
#' Gets data for rating-curve models. That is, constituent concentration and flows on river monitoring stations.
#' returns an object of class rcData
#' @param bBox bounding box, can use bbox()
#' @param charname characteristic name. See charNameLookup
getConcData <- function(bBox = NULL, charname = NULL, chartype = NULL,
                        statecode = NULL, ...) {
  if (!is.null(charname))
    charname = paste(charname, collapse = ";")
  ret1 <- readWQPdata(bBox = bBox, siteType = "Stream",
              characteristicName = URLencode(charname),
              sampleMedia = "Water", ...)
  ret2 <- wqp_checkClasses(ret1)
  ret2
}

#' Simple flow retrieval from WQP sites
#' @export
getFlowData <- function(siteids) {
  flowchars <- c("Flow", "Flow rate, instantaneous",
                 "Flow runoff", "Flow, runoff", "Storm water flow",
                 "Stream flow, instantaneous", "Stream flow, mean. daily",
                 "Discharge, River/Stream")

  f <- rep(1:(ceiling(length(siteids) / 100)), each = 100)[1:length(siteids)]
  siteIDsets <- split(siteids, f = f)
  urls <- lapply(siteIDsets, constructWQPURL, parameterCd = flowchars, "", "", FALSE)

  retlist <- lapply(urls, importWQP)

  out <- retlist # %>%
  lapply(wqp_checkClasses) %>%
    bind_rows()

  out
}



#' Takes an object returned by readWQPData (or getConcData) and returns an
#' object of class rcData
wqpToRcData <- function(concdata, flowdata) {


  concdf <- concdata %>%
    transmute(Date = ActivityStartDate,
              char = CharacteristicName,
              frac = ResultSampleFractionText,
              station = MonitoringLocationIdentifier,
              conc = ifelse(is.bdl,
                            DetectionQuantitationLimitMeasure.MeasureValue,
                            ResultMeasureValue),
              conc.units = ResultMeasure.MeasureUnitCode,
              is.bdl)
  flowdf <- flowdata %>%
    transmute(Date = ActivityStartDate,
              # flowchar = CharacteristicName,
              station = MonitoringLocationIdentifier,
              flow = ResultMeasureValue,
              flow.units = ResultMeasure.MeasureUnitCode) %>%
    group_by(station) %>%
    mutate(lqbar = mean(log(flow), na.rm = TRUE),
           lqsd = sd(log(flow), na.rm = TRUE))

  combodf <- inner_join(concdf, flowdf, by = c("Date", "station"))

  f <- with(combodf, make.names(paste(station, char, frac, conc.units)))
  rawlist <- split(combodf, f)
  means <- vapply(rawlist, function(x) x$lqbar[1], numeric(1))
  sds <- vapply(rawlist, function(x) x$lqsd[1], numeric(1))
  out <- Map(makeModelData, rawData = rawlist, qbar = means, qsd = sds) %>%
    lapply(arrange_, "Date")

}



# Units -------------------------------------------------------------------

#' @export
m3s_cfs <- function(x) x * 35.3147

#' @export
cfs_m3s <- function(x) x / 35.3147

#' @export
convertToCFS <- function(flow, units) {
  units <- tolower(units)
  mults <- c("ft3/sec" = 1,
             "gal/min" = 0.002229,
             "cfs" = 1,
             "ft3/s" = 1,
             "g/sec" = 0.13368)
  cfs <- flow * mults[units]
  cfs
}

#' Convert to milligrams per liter
#' @export
convertToMg_L <- function(conc, units) {
  mults <- c("ug/l" = 0.001,
             "ppb" = 0.001,
             "mg/l" = 1)
}

#' Wrapper for ud.convert that accepts vector inputs and multiple conversion candidates.
#' @param x Numeric vector of values
#' @param from Units of x values
#' @param to Units to convert to.
#' @return A data.frame with columns x and units
#' @details If length(to) > 0, units the first is tried first, followed by the second, etc.
#'  If units in from are already contained in to, these units are not converted.
#' @importFrom udunits2 ud.convert
#' @export

convertUnits <- function(x, from, to) {
  if (length(from) == 1)
    from <- rep(from, length(x))
  if (length(to) == 1)
    to <- rep(to, length(x))

  stopifnot(length(x) == length(from))

  from <- validateUnits(from)
  to <- validateUnits(to)

  # Perform a SINGLE conversion with nice error handling
  conv <- function(to_) {
    ret1 <- function(x_, from_) {
      ret2 <- try(ud.convert(x_, from_, to_), silent = TRUE)
      ret2 <- ifelse(is(ret2, "try-error"), NA_real_, ret2)
    }
    ret1
  }

  # vectorized conversion
  conv2 <- function(x_, from_, to_) {
    unlist(Map(conv(to_), x_ = x_, from_ = from_))
  }

  inds <- is.na(match(from, to))
  out <- data.frame(x = x, units = from, stringsAsFactors = FALSE)
  for (i in 1:length(to)) {
    if (sum(inds) == 0)
      break
    out[inds, ]$x = conv2(x[inds], from[inds], to[i])
    out[inds, ]$units <- to[i]
    inds <- is.na(out$x)

  }
  out
}

#' replacement function for commonly given, but "improper" units,
#' e.g. "CFS", which *should* be ft3/s
#' @export
validateUnits <- function(unit) {
  unit <- tolower(unit)
  replacement <- c("cfs" = "ft3/s")

  matches <- match(unit, names(replacement))
  matchna <- is.na(matches)
  out <- unit
  out[!matchna] <- replacement[matches[!matchna]]

  convertible <- vapply(out, udunits2::ud.is.parseable, logical(1))
  if (sum(!convertible) > 0)
    stop(paste("Unrecognized units:",
               paste(unique(out[!convertible]), collapse = ", ")))

  out
}


# WQP data checks ---------------------------------------------------------



#' Replaces missing units by
#' 1. Looking up using USGSPCode
#' 2. If value is zero, assigns units as most commonly used units in dataset
#' @param convertTo Optional character vector specifying the units to be converted to.
#' If more than one is provided, they are tried in the order they are given.
#' The udunits2 package is used for conversion.
#' @param wqpData data.frame returned by readWQPData
#' @export
wqp_checkUnits <- function(wqpData, convertTo = NULL) {

  badrows <- is.na(wqpData$ResultMeasure.MeasureUnitCode) |
    wqpData$ResultMeasure.MeasureUnitCode == ""

  # 1. lookup units using USGS pcode
  ptbl <- dataRetrieval::pCodeToName
  badpcodes <- wqpData$USGSPCode[badrows]
  badunits <- ptbl$measureunitcode[match(badpcodes, ptbl$parm_cd)]

  wqpData[badrows, ]$ResultMeasure.MeasureUnitCode <- badunits

  stillbad <- is.na(wqpData$ResultMeasure.MeasureUnitCode) |
    wqpData$ResultMeasure.MeasureUnitCode == ""

  # 2. for zeros, assign to most commonly used units for same constituent, fraction, station
  isZero <- wqpData$ResultMeasureValue == 0 &
    !is.na(wqpData$ResultMeasureValue)

  lookupUnitMode <- function(wd) {
    wd_smry <- wd %>%
      group_by(CharacteristicName,
               ResultSampleFractionText,
               MonitoringLocationIdentifier) %>%
      summarize(n = n())
    wqp_modes <- wqpData %>%
      dplyr::filter(CharacteristicName %in% wd_smry$CharacteristicName,
                    ResultSampleFractionText %in% wd_smry$ResultSampleFractionText,
                    MonitoringLocationIdentifier %in% wd_smry$MonitoringLocationIdentifier,
                    !is.na(ResultMeasure.MeasureUnitCode),
                    ResultMeasure.MeasureUnitCode != "") %>%
      group_by(CharacteristicName,
               ResultSampleFractionText,
               MonitoringLocationIdentifier) %>%
      summarize(mode = Mode(ResultMeasure.MeasureUnitCode)) %>%
      ungroup()
    wd1 <- left_join(wd, wqp_modes, by = c("MonitoringLocationIdentifier",
                                           "CharacteristicName",
                                           "ResultSampleFractionText")) %>%
      mutate(ResultMeasure.MeasureUnitCode = mode) %>%
      select(-mode)
    wd1$ResultMeasure.MeasureUnitCode
  }

  if (sum(stillbad & isZero) > 0)
    wqpData[stillbad & isZero, ]$ResultMeasure.MeasureUnitCode <-
    lookupUnitMode(wqpData[stillbad & isZero, ])

  stillbad2 <- is.na(wqpData$ResultMeasure.MeasureUnitCode) |
    wqpData$ResultMeasure.MeasureUnitCode == ""

  if (!is.null(convertTo)) {
    convertedVals <- convertUnits(x = wqpData$ResultMeasureValue,
                                  from = wqpData$ResultMeasure.MeasureUnitCode,
                                  to = convertTo)
    convertedDLs <- convertUnits(
      x = wqpData$DetectionQuantitationLimitMeasure.MeasureValue,
      from = wqpData$DetectionQuantitationLimitMeasure.MeasureUnitCode,
      to = convertTo)
    wqpData$ResultMeasureValue <- convertedVals$x
    wqpData$ResultMeasure.MeasureUnitCode <- convertedVals$units
    wqpData$DetectionQuantitationLimitMeasure.MeasureValue <-
      convertedDLs$x
    wqpData$DetectionQuantitationLimitMeasure.MeasureUnitCode <-
      convertedDLs$units
  }

  wqpData
}


#' Filters wqpData to include only allowed fractions
#' @param silent If FALSE (default) report how many rows and which fractions were omitted.
#' @param wqpData data.frame returned by readWQPData
#' @export
wqp_checkFraction <- function(wqpData, silent = FALSE) {
  allowedFractions <- c("Total", "Dissolved", "Recoverable", "Suspended", "Total Recoverable")
  keeprows <- wqpData$ResultSampleFractionText %in% allowedFractions
  out <- wqpData[keeprows, ]

  if (!silent && sum(keeprows) < nrow(wqpData)) {
    message(sprintf("Omitting %s rows with the following reported fractions: %s",
                    sum(!keeprows),
                    paste(unique(wqpData$ResultSampleFractionText[!keeprows]),
                          collapse = ", ")))
  }

  out
}

#' Make sure all columns are present and classes are correct.
#' @param wqpData data.frame returned by readWQPData
#' @export
wqp_checkClasses <- function(wqpData) {
  reference <- c("OrganizationIdentifier" = "character",
                 "OrganizationFormalName" = "character",
                 "ActivityIdentifier" = "character",
                 "ActivityTypeCode" = "character",
                 "ActivityMediaName" = "character",
                 "ActivityMediaSubdivisionName" = "character",
                 "ActivityStartDate" = "Date",
                 "ActivityStartTime.Time" = "character",
                 "ActivityStartTime.TimeZoneCode" = "character",
                 "ActivityEndDate" = "Date",
                 "ActivityEndTime.Time" = "character",
                 "ActivityEndTime.TimeZoneCode" = "character",
                 "ActivityDepthHeightMeasure.MeasureValue" = "numeric",
                 "ActivityDepthHeightMeasure.MeasureUnitCode" = "character",
                 "ActivityDepthAltitudeReferencePointText" = "character",
                 "ActivityTopDepthHeightMeasure.MeasureValue" = "numeric",
                 "ActivityTopDepthHeightMeasure.MeasureUnitCode" = "character",
                 "ActivityBottomDepthHeightMeasure.MeasureValue" = "numeric",
                 "ActivityBottomDepthHeightMeasure.MeasureUnitCode" = "character",
                 "ProjectIdentifier" = "character",
                 "ActivityConductingOrganizationText" = "character",
                 "MonitoringLocationIdentifier" = "character",
                 "ActivityCommentText" = "character",
                 "SampleAquifer" = "character",
                 "HydrologicCondition" = "character",
                 "HydrologicEvent" = "character",
                 "SampleCollectionMethod.MethodIdentifier" = "character",
                 "SampleCollectionMethod.MethodIdentifierContext" = "character",
                 "SampleCollectionMethod.MethodName" = "character",
                 "SampleCollectionEquipmentName" = "character",
                 "ResultDetectionConditionText" = "character",
                 "CharacteristicName" = "character",
                 "ResultSampleFractionText" = "character",
                 "ResultMeasureValue" = "numeric",
                 "ResultMeasure.MeasureUnitCode" = "character",
                 "MeasureQualifierCode" = "character",
                 "ResultStatusIdentifier" = "character",
                 "StatisticalBaseCode" = "character",
                 "ResultValueTypeName" = "character",
                 "ResultWeightBasisText" = "character",
                 "ResultTimeBasisText" = "character",
                 "ResultTemperatureBasisText" = "character",
                 "ResultParticleSizeBasisText" = "character",
                 "PrecisionValue" = "character",
                 "ResultCommentText" = "character",
                 "USGSPCode" = "character",
                 "ResultDepthHeightMeasure.MeasureValue" = "numeric",
                 "ResultDepthHeightMeasure.MeasureUnitCode" = "character",
                 "ResultDepthAltitudeReferencePointText" = "character",
                 "SubjectTaxonomicName" = "character",
                 "SampleTissueAnatomyName" = "character",
                 "ResultAnalyticalMethod.MethodIdentifier" = "character",
                 "ResultAnalyticalMethod.MethodIdentifierContext" = "character",
                 "ResultAnalyticalMethod.MethodName" = "character",
                 "MethodDescriptionText" = "character",
                 "LaboratoryName" = "character",
                 "AnalysisStartDate" = "Date",
                 "ResultLaboratoryCommentText" = "character",
                 "DetectionQuantitationLimitTypeName" = "character",
                 "DetectionQuantitationLimitMeasure.MeasureValue" = "numeric",
                 "DetectionQuantitationLimitMeasure.MeasureUnitCode" = "character",
                 "PreparationStartDate" = "Date",
                 "ProviderName" = "character",
                 "ActivityStartDateTime" = "POSIXct",
                 "ActivityEndDateTime" = "POSIXct")

  missingCols <- setdiff(names(reference), names(wqpData))
  if (length(missingCols) > 0)
    wqpData[missingCols] <- NA

  extraCols <- setdiff(names(wqpData), names(reference))
  if (length(extraCols) > 0)
    warning(paste("The following columns are unrecognized and have been removed:",
                  paste(extraCols, collapse = ", ")))

  wqpData <- wqpData[names(reference)]

  wrongClass <- !mapply(FUN = is, wqpData, reference[names(wqpData)])
  if (sum(wrongClass) > 0) {
    fixClass <- function(x, class)
      get(paste0("as.", class))(x)
    wqpData[wrongClass] <- Map(fixClass, x = wqpData[wrongClass],
                               class = reference[wrongClass])
  }

  wqpData
}

#' Check for detection limit behavior
#' Adds is.bdl column
#' Converts detection-limit units to MeasureValue units.
#' Augments DetectionQuantitationLimitMeasure.MeasureValue, DetectionQuantitationLimitMeasure.MeasureUnitCode
#' If detection limit not reported, assumes it is at largest non-bdl value for that dataset
#' @param wqpData data.frame returned by readWQPData
#' @export
wqp_checkBDL <- function(wqpData) {

  # Make detection limit and value have same units
  mismatches <- wqpData$ResultMeasure.MeasureUnitCode !=
    wqpData$DetectionQuantitationLimitMeasure.MeasureUnitCode &
    !is.na(wqpData$ResultMeasure.MeasureUnitCode) &
    !is.na(wqpData$DetectionQuantitationLimitMeasure.MeasureUnitCode)

  wqpData$DetectionQuantitationLimitMeasure.MeasureUnitCode[mismatches] <-
    wqpData$ResultMeasure.MeasureUnitCode[mismatches]
  wqpData$DetectionQuantitationLimitMeasure.MeasureValue[mismatches] <-
    with(wqpData[mismatches, ], mapply(ud.convert,
           x = DetectionQuantitationLimitMeasure.MeasureValue,
           u1 = DetectionQuantitationLimitMeasure.MeasureUnitCode,
           u2 = ResultMeasure.MeasureUnitCode))

  nonDetectStrings <- c("Present Below Quantification Limit", "*Non-detect",
                        "Not Detected", "Detected Not Quantified")
  strangeStrings <- setdiff(unique(na.omit(wqpData$ResultDetectionConditionText)),
                            nonDetectStrings)
  if(length(strangeStrings) > 0)
    stop(paste("Unrecognized detection limit condition text:",
               paste(strangeStrings, collapse = ", "),
               "Deal with these manually."))

  islt <- function(x, y)
    !is.na(x) & !is.na(y) & x < y
  belowReportedLimit <- islt(wqpData$ResultMeasureValue,
                             wqpData$DetectionQuantitationLimitMeasure.MeasureValue)
  reportedBelowLimit <- wqpData$ResultDetectionConditionText %in% nonDetectStrings
  wqpData$is.bdl <- belowReportedLimit | reportedBelowLimit


  # rows that have no useable detection limit info, yet are reported as BDL
  findBadRows <- function(wd) {
    noLimit <- with(wd,
                    is.na(DetectionQuantitationLimitMeasure.MeasureValue) |
                      is.na(DetectionQuantitationLimitMeasure.MeasureUnitCode) |
                      DetectionQuantitationLimitMeasure.MeasureUnitCode == "")
    wd$is.bdl & noLimit & !is.na(wd$is.bdl)
  }
  # May also be possible to get detection limit from ResultCommentText, but I
  # see only 1 observation out of 902 badrows where this would help.


  # set detection limit to maximum reported limit
  lookupDetLim <- function(wd) {
    wd_smry <- wd %>%
      group_by(CharacteristicName,
               ResultSampleFractionText,
               MonitoringLocationIdentifier) %>%
      summarize(n = n())

    # maximum detection limit reported for each dataset
    goodrows <- wqpData$is.bdl & !badrows # reported as below detection limit and have useable info
    wqp_maxdl <- wqpData[goodrows, ] %>%
      dplyr::filter(CharacteristicName %in% wd_smry$CharacteristicName,
                    ResultSampleFractionText %in% wd_smry$ResultSampleFractionText,
                    MonitoringLocationIdentifier %in% wd_smry$MonitoringLocationIdentifier) %>%
      group_by(CharacteristicName,
               ResultSampleFractionText,
               MonitoringLocationIdentifier) %>%
      summarize(maxdl = max(DetectionQuantitationLimitMeasure.MeasureValue),
                units = unique(DetectionQuantitationLimitMeasure.MeasureUnitCode)) %>% # May need better way to check units
      ungroup()

    # replace missing values with maximum reported
    wd1 <- left_join(wd, wqp_maxdl, by = c("MonitoringLocationIdentifier",
                                           "CharacteristicName",
                                           "ResultSampleFractionText")) %>%
      mutate(DetectionQuantitationLimitMeasure.MeasureValue = maxdl,
             DetectionQuantitationLimitMeasure.MeasureUnitCode = units,
             DetectionQuantitationLimitTypeName = "Inferred from maximum reported in dataset") %>%
      select(-maxdl, -units)
    wd1
  }

  # Set detection limit to minimum non-bdl value
  lookupMinDet <- function(wd) {
    wd_smry <- wd %>%
      group_by(CharacteristicName,
               ResultSampleFractionText,
               MonitoringLocationIdentifier) %>%
      summarize(n = n())

    # Minimum non-BDL value for each dataset
    wqp_mindet <- wqpData %>%
      dplyr::filter(!is.bdl,
                    CharacteristicName %in% wd_smry$CharacteristicName,
                    ResultSampleFractionText %in% wd_smry$ResultSampleFractionText,
                    MonitoringLocationIdentifier %in% wd_smry$MonitoringLocationIdentifier,
                    !is.na(ResultMeasure.MeasureUnitCode),
                    ResultMeasure.MeasureUnitCode != "") %>%
      group_by(CharacteristicName,
               ResultSampleFractionText,
               MonitoringLocationIdentifier) %>%
      summarize(mindet = min(ResultMeasureValue),
                units = unique(ResultMeasure.MeasureUnitCode)) %>% # May need better way to check units
      ungroup()

    # replace missing values with minimum reported above detection limit
    wd1 <- left_join(wd, wqp_mindet, by = c("MonitoringLocationIdentifier",
                                            "CharacteristicName",
                                            "ResultSampleFractionText")) %>%
      mutate(DetectionQuantitationLimitMeasure.MeasureValue = mindet,
             DetectionQuantitationLimitMeasure.MeasureUnitCode = units,
             DetectionQuantitationLimitTypeName = "Maximum reported above limit in dataset.") %>%
      select(-mindet, -units)
    wd1
  }

  # replace detlim, detlim units with maximum reported detection limit
  badrows <- findBadRows(wqpData)
  if (sum(badrows) > 0)
    wqpData[badrows, ] <- lookupDetLim(wqpData[badrows, ])

  # Replace remaining bad rows with minimum non-BDL value
  stillbad <- findBadRows(wqpData)
  if (sum(stillbad) > 0)
    wqpData[stillbad, ] <- lookupMinDet(wqpData[stillbad, ])

  wqpData
}

#' Check activity types
#' @param wqpData data.frame returned by readWQPData
#' @export
wqp_checkActivity <- function(wqpData) {
  knownTypes <- c("Sample-Routine", "Sample", "Not determined",
                  "Field Msr/Obs", "Sample-Composite Without Parents",
                  "Sample-Integrated Cross-Sectional Profile",
                  "Sample-Integrated Vertical Profile",
                  "Sample-Integrated Horizontal Profile",
                  "Sample-Field Split",
                  "Field Msr/Obs-Portable Data Logger")

  # Omit quality control samples
  wqpData <- wqpData[!grepl("Quality Control", wqpData$ActivityTypeCode), ]
  unrecTypes_conc <- setdiff(wqpData$ActivityTypeCode, knownTypes)
  if(length(unrecTypes_conc) > 0)
    warning(paste("Unknown activity types present in conc data:",
                  paste(unrecTypes_conc, collapse = "; ")))
  wqpData
}

#' ripped off from http://stackoverflow.com/a/8189441
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
