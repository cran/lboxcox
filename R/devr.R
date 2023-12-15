devr <- function(y.bin, y.pred){
  deviance_r <- matrix(NA, ncol = 1, nrow = length(y.bin))
  
  for(pp in 1:length(y.bin)){
    if(y.bin[pp] == 1){
      deviance_r[pp] = sqrt(-2*log(y.pred[pp]))
    }else if(y.bin[pp]==0){
      deviance_r[pp] = -sqrt(-2*log(1-y.pred[pp]))
    }
  }
  
  #replace inf and -inf to max and min 
  infidx = which(!is.finite(deviance_r))
  pinfidx = infidx[which(deviance_r[infidx]>0)]
  ninfidx = infidx[which(deviance_r[infidx]<0)]
  maxn = max(abs(deviance_r[is.finite(deviance_r)]))
  
  deviance_r[pinfidx] = maxn*1.1
  deviance_r[ninfidx] = -maxn*1.1
  dev.abs = colSums(abs(deviance_r))
  
  return(dev.abs)
}