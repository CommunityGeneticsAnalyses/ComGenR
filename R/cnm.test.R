cnm.test <- function(com,method='r1',nits=5000,burn=500,thin=10,threshold=0,null.out=FALSE){
  ###Co-occurrence Null Modeling test
  com[com<=threshold] <- 0
  com.nul <- nullCom(com,method=method,nits=nits,burn=burn,thin=thin)
  cs.nul <- unlist(pblapply(com.nul,cscore))
  cs.obs <- cscore(com)
  ses <- (cs.obs-mean(cs.nul))/sd(cs.nul)
  if (null.out){
    stats <- c(SES=ses,lower.p=length(cs.nul[cs.nul<=cs.obs])/length(cs.nul),
             upper.p=length(cs.nul[cs.nul>=cs.obs])/length(cs.nul))
    return(list(sim=com.nul,stats=stats))
  }else{
    return(c(SES=ses,lower.p=length(cs.nul[cs.nul<=cs.obs])/length(cs.nul),
             upper.p=length(cs.nul[cs.nul>=cs.obs])/length(cs.nul)))
  }
}
