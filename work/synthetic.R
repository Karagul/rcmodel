# Making synthetic concentration-flow data

library(lubridate)
library(dplyr)

set.seed(156)
dates = as.Date(ymd("2015-01-01") - months(1:240))
times = scale(as.numeric(dates), scale = 1)
doys = as.numeric(format(dates, "%j"))

flows = rlnorm(240, 5, 1.2)
q = scale(log(flows)) %>% as.vector()


# assemble
fc <- function(q, doy, time, plot = FALSE, seed = 156) {
  set.seed(seed)
  rlnorm(240, 5, 1.2)
  # select q relationship
  fq = function(q) q - q^2 / 2

  # select time relationship
  polycoefs = rnorm(5) / factorial(1:5) / 2
  pnl <- function(x, deg) (x /2113.175) ^ deg
  ft = function(t) sapply(1:5, pnl, x = time) %*% polycoefs


  # select doy relationship
  sincoefs = rnorm(5) / factorial(1:5) * 4
  coscoefs = rnorm(5) / factorial(1:5) * 4
  fd <- function(doy) {
    d = doy * pi / 180
    hmc <- function(deg, a, b) a * sin(d * deg) + b * cos(d * deg)
    Map(hmc, deg = 1:5, a = sincoefs, b = coscoefs) %>%
      as.data.frame() %>% unname() %>%
      apply(1, sum)
  }

  # constant
  const = -1

  if(plot) {
    oldpar = par(no.readonly = TRUE)
    on.exit({par(oldpar)})
    par(mfrow = c(2, 2), mar = c(4, 4, 1, 3))
    plot(q, fq(q))
    plot(times, ft(times))
    plot(doys, fd(doys))
    par(oldpar)
  }

  invisible(as.vector(fq(q)) + as.vector(fd(doy)) + as.vector(ft(time)) + const)
}

fc(q, doys, times, seed = 156, plot = TRUE)

rc_synth = data.frame(Date = dates, flow = flows, flow.units = "CFS",
                       q = q, doy = doys, time = times) %>%
  mutate(c_nonoise = fc(q, doy, time),
         noise = rnorm(240),
         c = c_nonoise + noise,
         conc = as.vector(exp(c)), conc.units = "mg/L",
         is.bdl = FALSE)
attr(rc_synth, "fcn") = fc

# make sure fcn attr works
foo <- attr(rc_synth, "fcn")
d2 = makeModelData(rc_synth)

# See how well a model performs
mod1 = rcgam(c ~ s(q, k = 5) + s(time) + s(doy, k = 4, bs = "cc"), d2)

summary(mod1)
plot(mod1, residuals = F, pages = 1)
crossvalidate(mod1, k = 30)
crossvalidate(mod1, k = 30, smear = FALSE)
plot(predict(mod1), rc_synth$conc)
plot(predict(mod1, smear = FALSE), rc_synth$conc)
abline(0, 1)

# Check out 2 high outliers
foo = rc_synth[rc_synth$conc > 1500, ]



pairs(rc_synth[c("Date", "flow", "conc")])
hist(rc_synth$conc)
car::qqPlot(rc_synth$conc)

synthrc = makeModelData(rc_synth)
pairs(synthrc)

# save
save(rc_synth, file = "data/rc_synth.rda")



conditionalParams <- function(date, flow, synthdat) {

  stopifnot(is.Date(date))
  synthrc = makeModelData(rc_synth)
  tfm = attr(synthrc, "transform")
  q = tfm$qtrans(flow)
  time = tfm$ttrans(date)
  doy = as.numeric(format(date, "%j"))


  logmean = fc(q, doy, time)
  logsd = 1
  list(logmean = logmean, logsd = logsd)
}

