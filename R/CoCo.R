#' Generate a vector of co-occurrence counts from an occurrence matrix
#' 
#' Models and plots a network model using the methods of Araujo et al. 2011.
#' 
#' 
#' @param x Community matrix with species in columns.
#' @param type Type of co-occurrence count matrix to return. 
#' @return Returns either co-occurrences (pos) or checkers (neg) counts for all unique non-recursive species pairs%% ~Describe the value returned .
#' @note %% ~~further notes~~
#' @author Matthew K. Lau
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.

CoCo <- function(x, type=c('pos', 'neg')){
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
