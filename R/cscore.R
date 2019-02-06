#' Calculates the C-Score of Stone and Roberts 1991.
#' 
#' Calculates the C-Score of Stone and Roberts 1991.
#' 
#' 
#' @param x Community matrix of species abundances or present absences with
#' species in columns.
#' @param cu.mat Logical: should the checkerboard unit matrix be returned?
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (x, cu.mat = FALSE) 
#' {
#'     x[x != 0] <- 1
#'     cu <- matrix(0, nrow = ncol(x), ncol = ncol(x))
#'     for (i in 1:ncol(x)) {
#'         for (j in 1:ncol(x)) {
#'             ri <- sum(x[, i])
#'             rj <- sum(x[, j])
#'             S <- x[, i] * 0
#'             S[x[, i] == 1 & x[, j] == 1] <- 1
#'             S <- sum(S)
#'             cu[i, j] <- (ri - S) * (rj - S)
#'         }
#'     }
#'     if (cu.mat) {
#'         return(cu)
#'     }
#'     else {
#'         return(mean(cu))
#'     }
#'   }
#' 
cscore <-
function(x,cu.mat=FALSE){
  x[x!=0] <- 1 #force binary
  cu <- matrix(0,nrow=ncol(x),ncol=ncol(x))
  for (i in 1:ncol(x)){
    for (j in 1:ncol(x)){
      ri <- sum(x[,i])
      rj <- sum(x[,j])
      S <- x[,i]*0
      S[x[,i]==1&x[,j]==1] <- 1
      S <- sum(S)
      cu[i,j] <- (ri-S)*(rj-S)
    }
  }
  if (cu.mat){return(cu)}else{return(mean(cu))}
}
