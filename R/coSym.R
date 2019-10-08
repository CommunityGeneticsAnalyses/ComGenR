#' Calculates the symmetry of species in a network.
#' 
#' Calculates node level statistics describing the symmetry of species
#' interactions in the network using the methods of Araujo et al. 2011.
#' @param x Dependency network.
#' @param zero.na LOGICAL: should zero values be removed?
#' @return Generates a measure of co-occurrence matrix symmetry.
#' @export coSym

coSym <- function(x = 'dependency network', zero.na = TRUE){
  out <- x * 0
  for (i in 1:nrow(x)){
      for (j in 1:ncol(x)){
          if (max(c(x[i,j],x[j,i]))==0){}else{
              out[i,j] <- abs(x[i,j]-x[j,i])/max(c(x[i,j],x[j,i]))
          }
      }
  }
  return(out)
}
