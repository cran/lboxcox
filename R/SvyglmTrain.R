#' @importFrom stats AIC
#' @importFrom stats as.formula
#' @importFrom survey svydesign
#' @importFrom foreach %dopar% foreach
#' @importFrom doParallel registerDoParallel
svyglm_train = function(formula, data, lambda_vector=seq(0, 2, length = 100), weight_column_name=NULL, num_cores=1){
  if (is.null(weight_column_name)){
    design = svydesign(ids=~1, data=data)
  }else{
    weight_formula = as.formula(paste("~", weight_column_name))
    design = svydesign(ids=~1, weights=weight_formula, data=data)
  }

  train_func = function(lambda){
    model = train_single_svyglm_model(formula, design, lambda)
    AIC(model, k=2)[2]
  }
  if (num_cores == 1){
    myAIC <- sapply(lambda_vector, train_func)
  }else{
    registerDoParallel(num_cores)
    lamb = 1 # this is useless but if it's not here the build process things "lamb" is undefined
    myAIC = foreach(lamb=lambda_vector) %dopar% {train_func(lamb)}
  }

  best_lambda = lambda_vector[which.min(myAIC)]
  best_model = train_single_svyglm_model(formula, design, best_lambda)
  best_model$lambda = best_lambda
  best_model
}

#' @importFrom survey svyglm
train_single_svyglm_model = function(formula, design, lambda){
  names = attr(terms(formula), "term.labels")
  myXX = design$variables[names[1]]
  if (lambda == 0) {myV0 <- log(myXX)}
  else {myV0 <- (myXX ^ lambda - 1) / lambda}

  design$variables[names[1]] <- myV0
  svyglm(formula, design=design)
}

#' @importFrom stats coef
get_inits_from_model = function(model){
  inits <- rep(NA, length(coef(model))+1)
  inits[1:2] <- coef(model)[1:2]
  inits[3] <- model$lambda
  inits[4:(length(coef(model))+1)] <- coef(model)[-c(1:2)]
  inits
}
