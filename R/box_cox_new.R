#' @title Box-Cox transform
#' @description This function processes the box cox transform on varibles
#' @param formula  a formula of the form y ~ x + z1 + z2 where y is a binary response variable, x is a continuous predictor variable, and z1, z2, ... are  covariates
#' @param mydata dataset used in box cox transform
#' @param ixx continuous predictor
#' @param lambda lambda used in box cox transform
#' @return data set after transform, contains transformed ixx
#' @export
box_cox_new = function(formula, mydata, ixx, lambda){
  names = attr(terms(formula), "term.labels")
  if(lambda != 0) {
    iv <- (ixx^lambda - 1)/lambda
  }else{iv = log(ixx)}
  
  # mydata.trans = cbind(mydata, iv)
  mydata = as.matrix(mydata)
  mydata[,names[1]] <- iv
  mydata.trans = as.data.frame(mydata)
  return(mydata.trans)
}