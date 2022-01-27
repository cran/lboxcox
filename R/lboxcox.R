#' @title Train a Logistic Box-Cox model
#' @description Train the given formula using a Logistic Box-Cox model.
#' @param formula a formula of the form y ~ x + z1 + z2 where y is a binary response variable, x is a continuous predictor variable, and z1, z2, ... are  covariates
#' @param weight_column_name the name of the column in `data` containing the survey weights.
#' @param data dataframe containing the dataset to train on
#' @param init initial estimates for the coefficients. If NULL the svyglm model will be used
#' @param svy_lambda_vector values of lambda used in training svyglm model. Best model is used for initial coefficient estimates. If init is not NULL this parameter is ignored.
#' @param num_cores the number of cores used when finding the best svyglm model. If init is not NULL this parameter is ignored.
#' @return object of class 'maxLik' from the 'maxLik' package. Contains the coefficient estimates that maximizes likelhood among other statistics.
#' @note This is reliant on the following work:
#'
#' Henningsen, A., Toomet, O. (2011). maxLik: A package for maximum likelihood estimation in R. Computational Statistics, 26(3), 443-458.
#'
#' Microsoft Corporation, Weston, S. (2020). foreach: Provides Foreach Looping Construct. R package version 1.5.1.
#'
#' Microsoft Corporation, Weston, S. (2020). doParallel: Foreach Parallel Adaptor for the 'parallel' Package. R package version 1.0.16.
#' @importFrom maxLik maxLik
#' @export
lbc_train = function(formula, weight_column_name, data, init=NULL, svy_lambda_vector=seq(0, 2, length = 100), num_cores=1){
  if (is.null(init)) {
    model = svyglm_train(formula, data, lambda_vector=svy_lambda_vector, weight_column_name=weight_column_name, num_cores=num_cores)
    init = get_inits_from_model(model)
  }
  processed_data = get_processed_data(formula, data, weight_column_name)
  maxLik(logLik=LogLikeFun, grad = ScoreFun, start=init, ixx=processed_data$ixx, iyy=processed_data$iyy, iw = processed_data$iw, iZZ = as.matrix(processed_data$iZZ) )
}

#' @importFrom stats terms
get_processed_data = function(formula, data, weight_column_name){
  variables = eval(attr(terms(formula), "variables"), envir=data)
  var_name_list = eval(attr(terms(formula), "term.labels"), envir=data)

  iyy = variables[[1]]
  ixx = variables[[2]]
  iZZ = data.frame(matrix(NA, nrow=length(iyy), ncol=0))

  for (idx in 3:length(variables)){
    var_name = var_name_list[idx-1]
    if (class(variables[[idx]]) == "factor"){
      iZZ = add1hot_encoding(iZZ, variables[[idx]], var_name)
    } else {
      iZZ[var_name] = variables[[idx]]
    }
  }
  mask = rowSums(apply(iZZ, 2, is.na)) == 0

  list(ixx=ixx[mask], iyy=iyy[mask], iZZ=iZZ[mask, ], iw=data[mask, weight_column_name])
}

add1hot_encoding = function(df, variable, var_name){
  for (i in 1:(length(levels(variable))-1)){
    new_name = paste(var_name, i, sep="_")
    df[new_name] = as.numeric(variable == i)
  }
  df
}
