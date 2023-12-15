#' @title Train a Logistic Box-Cox model using MaxLik
#' @description Train the given formula using a Logistic Box-Cox model.
#' @param formula a formula of the form y ~ x + z1 + z2 where y is a binary response variable, x is a continuous predictor variable, and z1, z2, ... are  covariates
#' @param weight_column_name the name of the column in `data` containing the survey weights.
#' @param data dataframe containing the dataset to train on
#' @param init initial estimates for the coefficients. If NULL the svyglm model will be used
#' @param svy_lambda_vector values of lambda used in training svyglm model. Best model is used for initial coefficient estimates. If init is not NULL this parameter is ignored.
#' @param init_lambda_vector values of lambda used in finding the optimal lambda with the best loglikelihood 
#' @param seed set seed for MaxLik function
#' @param num_cores the number of cores used when finding the best svyglm model. If init is not NULL this parameter is ignored.
#' @param iterlim Maximum number of iterations of MaxLik
#' @param timelim Maximum iteration time of MaxLik
#' @return object of class 'maxLik' from the 'maxLik' package. Contains the coefficient estimates that maximizes likelhood among other statistics.
#' @note This is reliant on the following work:
#'
#' Henningsen, A., Toomet, O. (2011). maxLik: A package for maximum likelihood estimation in R. Computational Statistics, 26(3), 443-458.
#'
#' Microsoft Corporation, Weston, S. (2020). foreach: Provides Foreach Looping Construct. R package version 1.5.1.
#'
#' Microsoft Corporation, Weston, S. (2020). doParallel: Foreach Parallel Adaptor for the 'parallel' Package. R package version 1.0.16.
#' @importFrom maxLik maxLik
#' @importFrom R.utils withTimeout
#' @importFrom dplyr if_else
#' @importFrom stats binomial
#' @export
# library(maxLik)
lbc_maxlik = function(formula, weight_column_name, data, init=NULL, svy_lambda_vector=seq(0, 2, length = 4), init_lambda_vector = seq(0, 2, length = 100),num_cores=1, seed,
                      iterlim, timelim){
  if(!is.null(weight_column_name)){
    weight = data[[weight_column_name]]
  }else{
    weight = NULL
  }
  
  if (is.null(init)) {
    model = svyglm_train(formula, data, lambda_vector=svy_lambda_vector, weight_column_name=weight, num_cores=num_cores)
    init = get_inits_from_model(model)
  }
  processed_data = get_processed_data(formula, data, weight)
  
  variables = eval(attr(terms(formula), "variables"), envir=data)
  variable_list = eval(attr(terms(formula), "term.labels"), envir=data)
  # covariate_names = variable_list[-1]
  
  set.seed(seed)
  result <- NULL
  tryCatch({
    result <- withTimeout({
      maxLik(logLik = LogLikeFun_new, grad = ScoreFun_new, start = init, ixx = processed_data$ixx, iyy = processed_data$iyy, iw = processed_data$iw, iZZ = as.matrix(processed_data$iZZ),
             control = list(iterlim = iterlim))
    }, timeout = timelim)
  }, TimeoutException = function(ex) {
    message("Timeout. Skipping.")
  })
  
  er <- if_else(
    is.null(result), 1, 0
  )
  
  init_list = svyglm_ms(formula, data, lambda_vector=init_lambda_vector, weight_column_name=weight, num_cores=num_cores)
  n = length(init_lambda_vector)
  mymax = matrix(NA, n, length(variables) + 2)
  for (ii in 1:n) {
    init = init_list[[ii]]
    mymax[ii,1:(length(variables) + 1)] = init
    
    temp.max  = try(LogLikeFun_new(init, ixx=processed_data$ixx, iyy=processed_data$iyy, iw = processed_data$iw, iZZ = as.matrix(processed_data$iZZ)),
                    silent = TRUE)
    
    if('try-error' %in% class(temp.max)){
      temp.max = -999999
    }
    if(is.na(temp.max)){
      temp.max = -999999
    }
    mymax[ii,(length(variables) + 2)] = temp.max
  }
  
  lambda = try(result$estimate[3])
  
  if(is.null(lambda) || lambda < 0 || lambda > 2){
    lambda = mymax[which.max(mymax[,(length(variables) + 2)]),3]
    mydata.trans = box_cox_new(formula, data, data[[variable_list[1]]], lambda)
    # design
    if (is.null(weight)){
      mydesign = svydesign(ids=~1, data=mydata.trans)
    }else{
      mydesign = svydesign(ids=~1, weights=weight, data=mydata.trans)
    }
    
    # fit svyglm regression 
    
    glm.all = svyglm(formula,design = mydesign,
                     family = binomial(link = "logit"))
    
    static_names = c("Beta_0", "Beta_1")
    covariate_names = names(processed_data$iZZ)
    glm_estimate <- glm.all$coefficients
    estimate = c(glm_estimate[1:2], lambda, glm_estimate[-c(1:2)])
    names(estimate) = c(static_names, "Lambda", covariate_names)
    glm.all$estimate = estimate
  }else{
    glm.all = result
    static_names = c("Beta_0", "Beta_1")
    covariate_names = names(processed_data$iZZ)
    names(glm.all$estimate) = c(static_names, "Lambda", covariate_names)
  } 
  glm.all$error = er
  glm.all
}

#' @title Calculates the "slope" of the Logistic Box-Cox model
#' @description Calculates a number that represents the overall gradient measurement between the predictor and log-odds of the risk
#' @param formula the formula used to train the logistic box-cox model
#' @param weight_column_name the name of the column in `data` containing the survey weights
#' @param data dataframe containing the dataset to train on
#' @param trained_model the already trained model. The output of `lbc_train`
#' @importFrom survey svydesign svymean
#' @importFrom MASS ginv
#' @export

median_effect = function(formula, weight_column_name, data, trained_model){
  weight_formula = as.formula(paste("~", weight_column_name))
  
  primary_predictor_name = eval(attr(terms(formula), "term.labels"), envir=data)[1]
  m = as.formula(paste0("~log(", primary_predictor_name, ")"))
  
  mysub = svydesign(ids=~1, weights=weight_formula, data = data)
  myu = svymean(m, mysub)[1]
  
  beta1 = trained_model$estimate[2]
  lambda = trained_model$estimate[3]
  median_effect = beta1 * exp((lambda - 1) * myu)
  mat_inverse = (-ginv(trained_model$hessian)[1:3,1:3]/dim(data)[1])
  varbeta1 = mat_inverse[2,2]
  covbeta1lambda = mat_inverse[2,3]
  varlambda = mat_inverse[3,3]
  standard_deviation = sqrt(median_effect ^ 2 * (varbeta1 / beta1 ^ 2 + 2 * myu * covbeta1lambda / beta1 + myu ^ 2 * varlambda))
  
  lower_ci = median_effect - 2 * standard_deviation
  upper_ci = median_effect + 2 * standard_deviation
  
  return_vec = c(median_effect, lower_ci, upper_ci)
  names(return_vec) = c("median effect", "lower 95% ci", "upper 95% ci")
  return_vec
}

#' @importFrom stats terms
library(stats)
get_processed_data = function(formula, data, weight_column_name){
  variables = eval(attr(terms(formula), "variables"), envir=data)
  var_name_list = eval(attr(terms(formula), "term.labels"), envir=data)
  
  iyy = variables[[1]]
  ixx = variables[[2]]
  iZZ = data.frame(matrix(NA, nrow=length(iyy), ncol=0))
  
  if(length(variables) >= 3){
    for (idx in 3:length(variables)){
      var_name = var_name_list[idx-1]
      if (is.factor(variables[[idx]])){
        iZZ = add1hot_encoding(iZZ, variables[[idx]], var_name)
      } else {
        iZZ[var_name] = variables[[idx]]
      }
    }
  }else{
    iZZ = data.frame(matrix(1, nrow=length(iyy), ncol=1))
  }
  mask = rowSums(apply(iZZ, 2, is.na)) == 0
  
  if(is.null(weight_column_name)){
    list(ixx=ixx[mask], iyy=iyy[mask], iZZ=iZZ[mask, ], iw=1)
  }else{list(ixx=ixx[mask], iyy=iyy[mask], iZZ=iZZ[mask, ], iw=weight_column_name[mask])}
}


add1hot_encoding = function(df, variable, var_name){
  for (i in 1:(length(levels(variable))-1)){
    new_name = paste(var_name, i, sep="_")
    df[new_name] = as.numeric(variable == i)
  }
  df
}



