#' @title Svyglm of MaxLik_ms
#' @description This function gives the initial value list used in MaxLik_ms function
#' @param formula formula used in model
#' @param data dataframe containing the dataset to train on
#' @param lambda_vector values of lambda used in training svyglm model. 
#' @param weight_column_name the name of the column in `data` containing the survey weights.
#' @param num_cores the number of cores used when finding the best svyglm model.
#' @return initial value list used in MaxLik_ms function
#' @export
svyglm_ms = function(formula, data, lambda_vector=seq(0, 2, length = 100), weight_column_name=NULL, num_cores=1){
  if (is.null(weight_column_name)){
    design = svydesign(ids=~1, data=data)
  }else{
    design = svydesign(ids=~1, weights=weight_column_name, data=data)
  }
  
  n = length(lambda_vector)
  myinit =  list()
  for (ii in 1:n) {
    model = train_single_svyglm_model_ms(formula, design, lambda_vector[ii])
    model$lambda = lambda_vector[ii]
    init = get_inits_from_model(model)
    myinit[[ii]] = init
  }
  myinit
}

#' @importFrom survey svyglm
#' @importFrom stats binomial
train_single_svyglm_model_ms = function(formula, design, lambda){
  names = attr(terms(formula), "term.labels")
  myXX = design$variables[names[1]]
  if (lambda == 0) {
    myV0 <- log(myXX)
  }else {
      myV0 <- (myXX ^ lambda - 1) / lambda
  }
  
  design$variables[names[1]] <- myV0
  survey::svyglm(formula, design=design, family = binomial(link = "logit"))
}

#' @importFrom stats coef
get_inits_from_model = function(model){
  inits <- rep(NA, length(coef(model))+1)
  inits[1:2] <- coef(model)[1:2]
  inits[3] <- model$lambda
  if((length(coef(model))+1) >= 4){
    inits[4:(length(coef(model))+1)] <- coef(model)[-c(1:2)]
  }
  inits
}



