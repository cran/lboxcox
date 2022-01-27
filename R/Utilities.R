LogLikeFun2 <- function(bb, ixx, iyy, iw, iZZ)
{
  # bb: initial values for the intercept and slope coefficients
  # ixx: continuous predictor
  # iyy: binary outcome
  # izz: covariates to be incorporated in the model

  myp <- length(bb)
  mycovbeta <- matrix(bb[3:myp], nrow = myp-2, ncol = 1)
  iS <- bb[1] + bb[2]*ixx + iZZ%*%mycovbeta
  eiS <- exp(-iS)
  eiS1 <- 1 + eiS
  # iP <- eiS/eiS1
  #sum((iyy*iS + log(eiS/eiS1)))
  sum(iw*(iyy*iS + log(eiS/eiS1)))/sum(sqrt(iw))
}

ScoreFun2 <- function(bb, ixx, iyy, iw, iZZ)
{
  # bb: initial values for the intercept and slope coefficients
  # ixx: continuous predictor
  # iyy: binary outcome

  myp <- length(bb)
  mycovbeta <- matrix(bb[3:myp], nrow = myp-2, ncol = 1)
  iS <- bb[1] + bb[2]*ixx + iZZ%*%mycovbeta
  eiS <- exp(-iS)
  iP <- matrix(1/(1 + eiS), nrow = 1)
  #iw <- 1/iw
  c(sum(iw*(iyy - iP)), sum(iw*((iyy - iP)*ixx)), (iw*(iyy-iP))%*%iZZ)/sum(iw)
}

HessFun <- function(bb, ixx, iyy, iw, iZZ){
  # bb: initial values for the intercept and slope coefficients
  # ixx: continuous predictor
  # iyy: binary outcome

  lamda <- bb[3]
  if(lamda != 0) {iv <- (ixx^lamda - 1)/lamda}
  else {iv = log(ixx)}
  iS <- bb[1] + bb[2]*iv
  eiS <- exp(-iS)
  eiS1 <- 1 + eiS
  iP <- 1/eiS1

  XlambdalogX <- ixx^lamda*log(ixx)
  P1P <- iP * (1-iP)

  if(lamda != 0)  { de.lamda <- (ixx^lamda*log(ixx) - iv)/lamda} # dv/dlambda
  else {de.lamda <- log(ixx)^2/2}
  depi.lambda <- P1P * bb[2] * de.lamda  # dP/dlambda
  if(lamda != 0)  {de2.lambda <- (XlambdalogX*log(ixx) - 2*XlambdalogX +2*iv)/(lamda^2)} # d2v/dlambda2


  hh00 <- -mean(P1P )
  hh01 <- -mean(P1P * iv )
  hh11 <- -mean(P1P * iv^2 )
  hh02 <- -mean(depi.lambda )
  #hh12 <-  mean((iyy - iP)*de.lamda - depi.lambda*iv)
  hh12 <-  mean(depi.lambda * iv)
  hh22 <- -mean(depi.lambda * bb[2] * de.lamda) + mean((iyy - iP) * bb[2] * de2.lambda)
  #hh22 <- -mean(depi.lambda * bb[2] * de.lamda)
  matrix(c(hh00, hh01, hh02, hh01, hh11, hh12, hh02, hh12, hh22), 3, 3)
}
