% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{splitSampleTest}
\alias{splitSampleTest}
\title{Conduct a differential split sample test on a regression model}
\usage{
splitSampleTest(object, condition, what = c("concentration", "load"),
  retransform = TRUE, scale = c("none", "gcv", "cv"), kfolds = 10,
  incl.data = FALSE, ...)
}
\arguments{
\item{object}{an rcgam object}

\item{condition}{a condition that returns logical when given a row of the
default newdata for predict(object)}

\item{what}{What to obtain errors for (concentration or load)}

\item{retransform}{Perform retransformation before calculating errors?}

\item{scale}{How to scale the residuals? (For comparing multiple models
for different datasets) Defaults to "none".}

\item{kfolds}{For scale == "cv" only. How many folds to use for crossvalidation?}

\item{incl.data}{Return data and scale along with residuals? If so, a list is returned.}

\item{...}{passed to predict.rcgam}
}
\description{
Returns prediction errors from a model calibrated for condition == FALSE and validated
for condition == TRUE. Based on differential split sample test described in
Klemes (1986)
}

