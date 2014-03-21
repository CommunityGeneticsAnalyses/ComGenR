###CoCo: Generate a vector of co-occurrence counts 
###from an occurrence matrix
###Returns either co-occurrences (pos) or checkers (neg) 
###counts for all unique non-recursive species pairs

CoCo <- function(x,type=c('pos','neg')){
  if (length(type)!=1|all(type!=c('pos','neg'))){print('Using positive co-occurrence');type='pos'}
  if (length(colnames(x))==0){colnames(x) <- paste('sp',1:ncol(x),sep='')}
  if (type=='neg'){print('Using negative co-occurrence')}
  x <- sign(x)
  y <- list()
  k <- 0
  for (i in 1:ncol(x)){
    for (j in i:ncol(x)){
      k <- k+1
      if (i!=j){
        if (type=='pos'){
          y[[k]] <- (x[,i]+x[,j])
          y[[k]][y[[k]]!=2] <- 0
          y[[k]] <- sign(y[[k]])
        }else{
          y[[k]] <- (x[,i]+x[,j])
          y[[k]][y[[k]]!=1] <- 0
          y[[k]] <- sign(y[[k]])
        }
        names(y)[k]=paste(colnames(x)[i],colnames(x)[j],sep='_')
      }
    }
  }
  y=do.call(cbind,y)
  return(y)
}
