# ## ---- include = FALSE---------------------------------------------------------
# knitr::opts_chunk$set(
#   collapse = TRUE,
#   comment = "#>"
# )
# 
# ## ----setup--------------------------------------------------------------------
# # library(lboxcox)
# # library(R.utils)
# # library(dplyr)
# 
# ## -----------------------------------------------------------------------------
# data(depress)
# head(depress)
# 
# ## ----model--------------------------------------------------------------------
# trained_model = lbc_maxlik(
#   depression ~ mercury + # response variable, and primary predictor
#     age + factor(gender), # covariates  
#   weight_column_name= "weight", # the column name which contains the information about survey weight,
#   #set weight_column_name = NULL if no weight, assuming equal probability
#   data=depress, # data comes from package
#   svy_lambda_vector = seq(0, 2, length = 25),
#   init_lambda_vector = seq(0, 2, length = 100),
#   num_cores=2, # since vignettes can only work with a max of two processes
#   seed = 1
# )
# summary(trained_model)
# 
# # pred<- lboxcox_maxLik.predict(trained_model, depress, depression ~ mercury)
# # devr(depress$depression, pred)
# 
# trained_model_ms = lbc_train_ms(
#   depression ~ mercury + # response variable, and primary predictor
#     age + factor(gender), # covariates
#   weight_column_name="weight", # the column name which contains the information about survey weight
#   data=depress, # data comes from package
#   svy_lambda_vector = seq(0, 2, length = 100),
#   num_cores=2 # since vignettes can only work with a max of two processes
# )
# summary(trained_model_ms)
# trained_model_ms$estimate
# 
# # pred_ms <- lboxcox_maxLik.predict(trained_model, depress, depression ~ mercury + age + factor(gender))
# # devr(depress$depression, pred_ms)
# 
# 
# 
# trained_model_el = lbc_train_bagging(
#   depression ~ mercury + # response variable, and primary predictor
#     age + factor(gender), # covariates
#   weight_column_name="weight", # the column name which contains the information about survey weight
#   data=depress, # data comes from package
#   svy_lambda_vector = seq(0, 2, length = 100),
#   init_lambda_vector = seq(0, 2, length = 100),
#   nbagging = 10, # number of subsets
#   num_cores=1 # since vignettes can only work with a max of two processes
# )
# summary(trained_model_el[[1]])
# trained_model_el[[1]]$estimate
# # summary(trained_model_el)
# 
# # pred_el <- lboxcox_maxLik_el.predict(trained_model_el, depress, 
# #                                         formula = depression ~ mercury +
# #                                           age + factor(gender))
# 
# 
# ## ----model_init---------------------------------------------------------------
# init = c(0.0984065403, -0.0227734374, 0.0000000000, -0.0002426025, -0.0316484585)
# trained_model = lbc_maxlik(
#   depression ~ mercury + # response variable, and primary predictor
#     age + factor(gender), # covariates
#   weight_column_name="weight", # the column name which contains the information about survey weight
#   data=depress, # data comes from package
#   init=init, # initial guess for the coefficients
#   seed = 1
# )
# summary(trained_model)
# 
# 
# 
# trained_model_ms = lbc_train_ms(
#   depression ~ mercury + # response variable, and primary predictor
#     age + factor(gender), # covariates
#   weight_column_name="weight", # the column name which contains the information about survey weight
#   data=depress, # data comes from package
#   svy_lambda_vector = seq(0, 2, length = 100),
#   num_cores=2, # since vignettes can only work with a max of two processes
#   init=init # initial guess for the coefficients
# )
# summary(trained_model_ms)
# trained_model_ms$estimate
# 
# 
# 
# trained_model_el = lbc_train_bagging(
#   depression ~ mercury + # response variable, and primary predictor
#     age + factor(gender), # covariates
#   weight_column_name="weight", # the column name which contains the information about survey weight
#   data=depress, # data comes from package
#   svy_lambda_vector = seq(0, 2, length = 100),
#   init_lambda_vector = seq(0, 2, length = 100),
#   nbagging = 10, # number of subsets
#   num_cores=2, # since vignettes can only work with a max of two processes
#   init=init # initial guess for the coefficients
# )
# summary(trained_model_el[[1]])
# trained_model_el[[1]]$estimate

