#' Make model data from "raw" data
#'
#' @param rawData The data to convert. Currently methods only exist for
#' data.frame, but rcData objects may be added in the future.
#' @param model (optional) A model from which to extract transformation
#' information.
#' @param flowTransform Which transformation to use (link argument in
#' stats::make.link)
#' @param concTransform Which transformation to use (link argument in
#' stats::make.link)
#' @param qbar (optional) mean to use for flow scaling post-transform.
#' @param qsd (optional) stdev to use for flow scaling post-transform.
#' @param cbar (optional) mean to use for concentration scaling post-transform.
#' @param csd (optional) stdev to use for concentration scaling post-transform.
#' @export


makeModelData <- function(rawData, ...) {
  UseMethod("makeModelData")
}

#' @export

makeModelData.data.frame <- function(rawData, model = NULL,
                                     flowTransform = "log",
                                     concTransform = "log",
                                     qbar = NULL, qsd = NULL,
                                     cbar = NULL, csd = NULL) {

  neededCols <- c("Date", "flow", "flow.units", "conc", "conc.units", "is.bdl")
  diffs = setdiff(neededCols, names(rawData))
  if(length(diffs) > 0) stop(paste("rawData is missing the following needed column(s):",
                                   paste(diffs, collapse = ", ")))

  qunits = as.character(unique(rawData[["flow.units"]]))
  cunits = as.character(unique(rawData[["conc.units"]]))
  if(length(qunits) != 1) stop(paste("Data must all have the same flow units:", qunits))
  if(length(cunits) != 1) stop(paste("Data must all have the same conc units:", cunits))

  rawData[["Date"]] = as.Date(rawData[["Date"]])

  with(rawData, assertthat::assert_that(is(flow, "numeric"),
                            is(conc, "numeric"),
                            is(is.bdl, "logical")))

  if(!is.null(model)) {
    stopifnot(is(model, "rcgam"))
    assertthat::assert_that(all(model$units$qunits == rawData$flow.units),
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
    qbar <- ifelse(is.null(qbar), attr(q_scale, "scaled:center"), qbar)
    qsd  <- ifelse(is.null(qsd), attr(q_scale, "scaled:scale"), qsd)
    qtransform = transf(flowLink, qbar, qsd)

    c_scale = scale(concLink$linkfun(rawData[["conc"]]))
    cbar <- ifelse(is.null(cbar), attr(c_scale, "scaled:center"), cbar)
    csd <- ifelse(is.null(csd), attr(c_scale, "scaled:scale"), csd)
    ctransform = transf(concLink, cbar, csd)

    datebar = mean(rawData$Date)
    ttransform = transf(rawData$Date, datebar)

    modelData <- rawData %>%
      mutate_(time = ~ as.numeric(Date) - as.numeric(datebar),
              doy = ~ as.numeric(format(Date, "%j")),
              q = ~ qtransform$trans(flow),
              c = ~ ctransform$trans(conc)) %>%
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


#' Get data from rcgam objects
#'
#' simple extraction of data, returning useful errors if impossible. Useful as a
#' method for the generic `getData` from `nlme` package
#'
#' @param object an object of class `rcgam`
#' @param type What kind of data to return--raw or transformed (rcData object)
#' @export
#' @importFrom markstats getData

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

#' @export
`[.rcData` <- function(x, i, ...) {
  r <- NextMethod("[")
  ats <- attributes(x)
  ats$names = names(r)
  ats$row.names = 1:nrow(r)
  mostattributes(r) <- ats
  r
}

#' Calculate loads from concentration and flow
#'
#' Performs appropriate unit conversion to go from flow and concentration to load
#'
#' @export

calcLoad <- function(flow, conc, flow.units = "CFS", conc.units = "mg/l", load.units = "kg/day") {
  if(flow.units != "CFS" || conc.units != "mg/l" || load.units != "kg/day")
    stop("units other than CFS, mg/l, kg/day not currently supported. Must use defaults for now.")

  siflowconst <- c("CFS" = 28.3168466)
  siconcconst <- c("mg/l" = 1 / 1000000)
  siloadconst <- c("kg/day" = 1 / (24 * 3600))

  out = flow * siflowconst[flow.units] * conc * siconcconst[conc.units] / siloadconst[load.units]
  unname(out)
}



