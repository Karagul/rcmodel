#' Make model data from "raw" data
#'
#'
#'
#'


makeModelData <- function(rawData) UseMethod("makeModelData")

#
# make model data from a data frame
#

makeModelData.data.frame <- function(rawData, model = NULL,
                                     flowTransform = "log",
                                     concTransform = "log") {

  neededCols <- c("Date", "flow", "flow.units", "conc", "conc.units", "is.bdl")
  diffs = setdiff(neededCols, names(rawData))
  if(length(diffs) > 0) stop(paste("rawData is missing the following needed column(s):", diffs))

  qunits = unique(rawData[["flow.units"]])
  cunits = unique(rawData[["conc.units"]])
  if(length(qunits) != 1) stop(paste("Data must all have the same flow units:", qunits))
  if(length(cunits) != 1) stop(paste("Data must all have the same conc units:", cunits))

  rawData[["Date"]] = as.Date(rawData[["Date"]])

  with(rawData, assert_that(is(flow, "numeric"),
                            is(conc, "numeric"),
                            is(is.bdl, "logical")))

  if(!is.null(model)) {
    stopifnot(is(model, "rcgam"))
    assert_that(all(model$units$qunits == rawData$flow.units),
                all(model$units$cunits == rawData$conc.units))
    tf = model$transform
    modelData <- rawData %>%
      mutate_(q = ~tf$qtrans(rawData$flow),
              c = ~tf$ctrans(rawData$conc)) %>%
      select_("-flow", "-flow.units", "-conc", "-conc.units")
    stats = model$stats
    translist = model$transform
    units = model$units
  } else {

    # transform flow and conc, obtain stats
    flowLink = stats::make.link(flowTransform)
    concLink = stats::make.link(concTransform)

    q_scale = scale(flowLink$linkfun(rawData[["flow"]]))
    qbar = attr(q_scale, "scaled:center")
    qsd  = attr(q_scale, "scaled:scale")
    qtransform = transf(flowLink, qbar, qsd)

    c_scale = scale(concLink$linkfun(rawData[["conc"]]))
    cbar = attr(c_scale, "scaled:center")
    csd  = attr(c_scale, "scaled:scale")
    ctransform = transf(concLink, cbar, csd)

    datebar = mean(rawData$Date)
    ttransform = transf(rawData$Date, datebar)

    modelData <- rawData %>%
      mutate_(time = ~ as.numeric(Date) - as.numeric(datebar),
              doy = ~ as.numeric(format(Date, "%j")),
              q = ~ as.vector(q_scale),
              c = ~ as.vector(c_scale)) %>%
      select_("-flow", "-flow.units", "-conc", "-conc.units")

    translist <- list(qtrans = qtransform$trans,
                      qinvert = qtransform$invert,
                      ctrans = ctransform$trans,
                      cinvert = ctransform$invert,
                      ttrans = ttransform$trans,
                      tinvert = ttransform$invert)
    units = c(qunits = qunits, cunits = cunits)
  }

  structure(modelData, class = c("rcData", "data.frame"),
            stats = c(datebar = datebar, qbar = qbar, qsd = qsd,
                      cbar = cbar, csd = csd),
            transform = translist,
            units = units)
}


#
# Make model data from a wqData object (not implemented yet)
#

makeModelData.wqData <- function(rawData) {
  stop("Not implemented yet")
}


#
#' Make raw data frame from a rcData object
#'
#' Generic function for getting untransformed data from transformed data and
#' some specification of transformation.
#'
#' @param data transformed data.
#' @param tfm a list containing transformation functions, not necessary if data is an rcData object.

makeRawData <- function(data, ...) UseMethod("makeRawData")

makeRawData.rcData <- function(data) {
  du <- attr(data, "units")
  tfm <- attr(data, "transform")

  rawData <- data %>%
    mutate_(flow = ~ tfm$qinvert(q),
            flow.units = ~ du["qunits"],
            conc = ~ tfm$cinvert(c),
            conc.units = ~ du["cunits"], is.bdl = ~ is.bdl,
            Date = ~ tfm$tinvert(time)) %>%
    select_("-c", "-q", "-time")
  rawData
}

makeRawData.data.frame <- function(data, tfm, units) {
  rawData <- data %>%
    mutate_(flow = ~ tfm$qinvert(q),
            flow.units = ~ units["qunits"],
            conc = ~ tfm$cinvert(c),
            conc.units = ~ units["cunits"], is.bdl = ~ is.bdl,
            Date = ~ tfm$tinvert(time)) %>%
    select_("-c", "-q", "-time")
  rawData
}


#
#' Transform data in a systematic way
#'
#' Use foo = stats::make.link(); then foo$linkfun(mu), foo$linkinv(eta)
#' foo = stats::power works the same way
#' in the future, could customize following http://stackoverflow.com/questions/15931403/modify-glm-function-to-adopt-user-specified-link-function-in-r

transf <- function(object, ...) {
  UseMethod("transf")
}

`transf.link-glm` <- function(object, center, scale) {
  force(center); force(scale)
  trans <- function(x) (object$linkfun(x) - center) / scale
  invert <- function(x) object$linkinv((x * scale) + center)
  list(trans = trans, invert = invert)
}

#' Obtain functions to transform Date object to and from scaled numeric

transf.Date <- function(object, center, scale = 1) {
  force(center); force(scale)
  trans <- function(x) (as.numeric(x) - as.numeric(center)) / scale
  invert <- function(x) as.Date((x * scale) + center, origin = "1970-01-01")
  list(trans = trans, invert = invert)
}


getData <- function(object, ...) {
  UseMethod("getData")
}

#' Get data from rcgam objects
#'
#' simple extraction of data, returning useful errors if impossible. Useful as a
#' method for the generic `getData` from `nlme` package
#'
#' @param object an object of class `rcgam`
#' @param type What kind of data to return--raw or transformed (rcData object)
#' @export
#'

getData.rcgam <- function(object, type = c("raw", "rcData")) {
  type = match.arg(type)

  out <- if (is.null(object$data)) {
    warning("model structure does not include data. Attempting to get from environment")
    eval(object$call$data, envir = attr(object$terms, ".Environment"))
  }
  else
    object$data

  if(type == "raw")
    out <- makeRawData(out)
  out
}


`[.rcData` <- function(x, i, ...) {
  r <- NextMethod("[")
  ats <- attributes(x)
  ats$names = names(r)
  ats$row.names = 1:nrow(r)
  mostattributes(r) <- ats
  r
}
