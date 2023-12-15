## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning=FALSE, 
  message=FALSE
)

## ----setup--------------------------------------------------------------------
library(lboxcox)

## -----------------------------------------------------------------------------
data(depress)
head(depress)

## ----model--------------------------------------------------------------------
trained_model = lbc_maxlik(
  depression ~ mercury + # response variable, and primary predictor
  age + factor(gender), # covariates
  weight_column_name="weight", # the column name which contains the information about survey weight 
  data=depress, # data comes from package
  svy_lambda_vector = seq(0, 2, length = 25),
  init_lambda_vector = seq(0, 2, length = 100),
  num_cores=2, # since vignettes can only work with a max of two processes
  seed = 1,
  iterlim = 100,
  timelim = 10
)
summary(trained_model)

## ----model_init---------------------------------------------------------------
init = c(0.0984065403, -0.0227734374, 0.0000000000, -0.0002426025, -0.0316484585)
trained_model = lbc_maxlik(
  depression ~ mercury + # response variable, and primary predictor
  age + factor(gender), # covariates
  weight_column_name="weight", # the column name which contains the information about survey weight
  data=depress, # data comes from package
  svy_lambda_vector = seq(0, 2, length = 25),
  init_lambda_vector = seq(0, 2, length = 100),
  num_cores=2, # since vignettes can only work with a max of two processes
  seed = 2023,
  iterlim = 100,
  timelim = 10
)
summary(trained_model)

