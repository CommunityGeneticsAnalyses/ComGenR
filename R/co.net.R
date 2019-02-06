#' Co-occurrence network function.
#' 
#' Conducts network modeling using species abundances via the methods of Araujo
#' et al. 2011.
#' 
#' All values are converted to present absences within the function. Not using
#' abundance values allows for a different perspective and computational
#' simplicity as compared to using true abundance data.
#' 
#' @param x A community matrix with species in columns.
#' @param diag.zero
#' @param std %% ~~Describe \code{std} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
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
#' 
#' ## The function is currently defined as
#' function (x = "species in cols", diag.zero = TRUE, std = TRUE) 
#' {
#'     x[x != 0] <- 1
#'     out <- matrix(NA, nrow = ncol(x), ncol = ncol(x))
#'     rownames(out) <- colnames(out) <- colnames(x)
#'     for (j in 1:ncol(x)) {
#'         for (k in 1:ncol(x)) {
#'             out[j, k] <- null.prune(x[, j], x[, k], std = std)
#'         }
#'     }
#'     if (diag.zero) {
#'         diag(out) <- 0
#'     }
#'     else {
#'     }
#'     return(out)
#'   }
#' 
co.net <-
function(x='species in cols',diag.zero=TRUE,std=TRUE){
  x[x!=0] <- 1
  out <- matrix(NA,nrow=ncol(x),ncol=ncol(x))
  rownames(out) <- colnames(out) <- colnames(x)
  for (j in 1:ncol(x)){
    for (k in 1:ncol(x)){
      out[j,k] <- null.prune(x[,j],x[,k],std=std)
    }
  }
  if (diag.zero){diag(out) <- 0}else{}
  return(out)
}
