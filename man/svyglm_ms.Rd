% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/svyglm_ms.R
\name{svyglm_ms}
\alias{svyglm_ms}
\title{Svyglm of MaxLik_ms}
\usage{
svyglm_ms(
  formula,
  data,
  lambda_vector = seq(0, 2, length = 100),
  weight_column_name = NULL,
  num_cores = 1
)
}
\arguments{
\item{formula}{formula used in model}

\item{data}{dataframe containing the dataset to train on}

\item{lambda_vector}{values of lambda used in training svyglm model.}

\item{weight_column_name}{the name of the column in `data` containing the survey weights.}

\item{num_cores}{the number of cores used when finding the best svyglm model.}
}
\value{
initial value list used in MaxLik_ms function
}
\description{
This function gives the initial value list used in MaxLik_ms function
}
