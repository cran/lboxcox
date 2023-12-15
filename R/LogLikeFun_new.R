#' @title New Log Likelihood of Logistic Box-Cox
#' @description This function gives the log likelihood of the Box-Cox model.
#' Main purpose is to be an input to the maxLik function.
#' @param bb current values for the intercept and slope coefficients
#' @param ixx continuous predictor
#' @param iyy binary outcome
#' @param iw sample weight
#' @param iZZ covariates to be incorporated in the model
#' @return the log likelihood estimate for the coefficients in `bb`
#' @export
LogLikeFun_new <- function(bb, ixx, iyy, iw, iZZ){
  lamda <- bb[3]
  myp <- length(bb)
    if(myp > 3){
      mycovbeta <- matrix(bb[4:myp], nrow = myp-3, ncol = 1)
      if(lamda != 0){
        iv <- (ixx^lamda - 1)/lamda
      } else{
        iv = log(ixx)
      }
      iS <- bb[1] + bb[2]*iv + iZZ%*%mycovbeta
      eiS <- exp(-iS)
      eiS1 <- 1 + eiS
      # sum(iw*(iyy*iS + log(eiS/eiS1)))/sum(sqrt(iw))
      sum(iw*(iyy*iS + log(1 - 1/eiS1)))/sum(sqrt(iw))
    }else{
      if(lamda != 0){
        iv <- (ixx^lamda - 1)/lamda
      } else{
        iv = log(ixx)
      }
      iS <- bb[1] + bb[2]*iv
      eiS <- exp(-iS)
      eiS1 <- 1 + eiS
      # sum(iw*(iyy*iS + log(eiS/eiS1)))/sum(sqrt(iw))
      sum(iw*(iyy*iS + log(1 - 1/eiS1)))/sum(sqrt(iw))
    }
}
