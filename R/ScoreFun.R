#' @title Log Likelihood Gradient of Logistic Box-Cox
#' @description This function gives the gradient of the log likelihood of the Box-Cox model.
#' Main purpose is to be an input to the maxLik function.
#' @param bb initial values for the intercept and slope coefficients
#' @param ixx continuous predictor
#' @param iyy binary outcome
#' @param iw sample weight
#' @param iZZ covariates to be incorporated in the model
#' @return the gradient of the log likelihood estimate for the coefficients in `bb`
#' @export
ScoreFun <- function(bb, ixx, iyy, iw, iZZ){
  lamda <- bb[3]
  myp <- length(bb)
  mycovbeta <- matrix(bb[4:myp], nrow = myp-3, ncol = 1)
  if(lamda != 0) {iv <- (ixx^lamda - 1)/lamda}
  else {iv = log(ixx)}
  iS <- bb[1] + bb[2]*iv + iZZ%*%mycovbeta
  eiS <- exp(-iS)
  iP <- matrix(1/(1 + eiS), nrow = 1)
  if(lamda != 0)  { de.lamda <- (ixx^lamda*log(ixx) - iv)/lamda} # dv/dlambda
  else {de.lamda <- log(ixx)^2/2}
  #iw <- 1/iw
  c(sum(iw*(iyy - iP)), sum(iw*((iyy - iP)*iv)), sum(iw*((iyy - iP)*bb[2]*de.lamda)),
    (iw*(iyy-iP))%*%iZZ)/sum(iw)
}
