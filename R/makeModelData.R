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
    # transform flow and conc
    flowLink = stats::make.link(flowTransform)
    concLink = stats::make.link(concTransform)

    q_scale = scale(flowLink$linkfun(rawData[["flow"]]))
    qbar = attr(q_scale, "scaled:center")
    qsd  = attr(q_scale, "scaled:scale")
    qtransform = transform(flowLink, qbar, qsd)

    c_scale = scale(concLink$linkfun(rawData[["conc"]]))
    cbar = attr(c_scale, "scaled:center")
    csd  = attr(c_scale, "scaled:scale")
    ctransform = transform(concLink, cbar, csd)

    modelData <- rawData %>%
      mutate_(q = ~ as.vector(q_scale),
              c = ~ as.vector(c_scale)) %>%
      select_("-flow", "-flow.units", "-conc", "-conc.units")

    translist <- list(qtrans = qtransform$trans,
                      qinvert = qtransform$invert,
                      ctrans = ctransform$trans,
                      cinvert = ctransform$invert)
    units = c(qunits = qunits, cunits = cunits)
  }

  structure(modelData, class = c("rcData", "data.frame"),
            stats = c(qbar = qbar, qsd = qsd, cbar = cbar, csd = csd),
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
# Make raw data frame from a rcData object
#

makeRawData <- function(data) UseMethod("makeRawData")

makeRawData.rcData <- function(data) {
  ds <- attr(data, "stats"); du <- attr(data, "units")
  ql <- attr(data, "qtrans"); cl <- attr(data, "ctrans")
  tfm <- attr(data, "transform")
  rawData <- data %>%
    mutate_(flow = ~ tfm$qinvert(q),
            flow.units = ~ du["qunits"],
            conc = ~ tfm$cinvert(c),
            conc.units = ~ du["cunits"], is.bdl = ~ is.bdl) %>%
    select_("-c", "-q")
  rawData
}

#
# Transform data in a systematic way
# Use foo = stats::make.link(); then foo$linkfun(mu), foo$linkinv(eta)
# foo = stats::power works the same way
# in the future, could customize following http://stackoverflow.com/questions/15931403/modify-glm-function-to-adopt-user-specified-link-function-in-r
#

transform <- function(linkObj, center, scale) {
  force(center); force(scale)
  trans <- function(x) (linkObj$linkfun(x) - center) / scale
  invert <- function(x) linkObj$linkinv((x * scale) + center)
  list(trans = trans, invert = invert)
}

