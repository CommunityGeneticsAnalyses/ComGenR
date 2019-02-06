#' Calculates the symmetry of species in a network.
#' 
#' Calculates node level statistics describing the symmetry of species
#' interactions in the network using the methods of Araujo et al. 2011.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x %% ~~Describe \code{x} here~~
#' @param zero.na %% ~~Describe \code{zero.na} here~~
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
#' function (x = "dependency network", zero.na = TRUE) 
#' {
#'     out <- x * 0
#'     for (i in 1:nrow(x)) {
#'         for (j in 1:ncol(x)) {
#'             if (max(c(x[i, j], x[j, i])) == 0) {
#'             }
#'             else {
#'                 out[i, j] <- abs(x[i, j] - x[j, i])/max(c(x[i, 
#'                   j], x[j, i]))
#'             }
#'         }
#'     }
#'     return(out)
#'   }
#' 
coSym <-
function(x='dependency network',zero.na=TRUE){
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
