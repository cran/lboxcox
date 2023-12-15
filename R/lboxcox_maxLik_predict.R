#' @title Lboxcox MaxLik Prediction Function
#' @description Give the predicted p value of given LBC MaxLik model
#' @param myMaxLikfit Fitted model using lboxcox_maxLik model
#' @param newdata Given data for prediction
#' @param formula a formula of the form y ~ x + z1 + z2 where y is a binary response variable, x is a continuous predictor variable, and z1, z2, ... are  covariates
#' @return p value
#' @note This is reliant on the following work:
#' @export

###########################################
#
# lboxcox maxLik prediction
#
###########################################

lboxcox_maxLik.predict <- function(myMaxLikfit, newdata, formula){
  variables = eval(attr(terms(formula), "variables"), envir=newdata)
  var_name_list = eval(attr(terms(formula), "term.labels"), envir=newdata)
  estimate = myMaxLikfit$estimate
  if(length(variables) < 3){
    Beta0 = myMaxLikfit$estimate["Beta_0"]
    Beta1 = myMaxLikfit$estimate["Beta_1"]
    lambda = myMaxLikfit$estimate["Lambda"]
    
    newdata.trans = box_cox_new(formula, newdata, newdata[[var_name_list[1]]], lambda)
    a = exp(Beta0 + Beta1 * newdata.trans[[var_name_list[1]]])
    p = a / (1 + a)
  }else{
    iZZ = data.frame(matrix(NA, nrow=length(variables[[1]]), ncol=0))
    for (idx in 3:length(variables)){
      var_name = var_name_list[idx-1]
      if (is.factor(variables[[idx]])){
        iZZ = add1hot_encoding(iZZ, variables[[idx]], var_name)
      } else {
        iZZ[var_name] = variables[[idx]]
      }
    }
    
    Beta0 = myMaxLikfit$estimate["Beta_0"]
    Beta1 = myMaxLikfit$estimate["Beta_1"]
    lambda = myMaxLikfit$estimate["Lambda"]
    
    covariate = matrix(estimate[c(4:length(estimate))], nrow = length(estimate[c(4:length(estimate))]), ncol = 1)
    newdata.trans = box_cox_new(formula, newdata, newdata[[var_name_list[1]]], lambda)
    a = exp(Beta0 + Beta1 * newdata.trans[[var_name_list[1]]] + as.matrix(iZZ)%*%covariate)
    p = a / (1 + a)
  }
  return(p)
}





