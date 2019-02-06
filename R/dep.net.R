#' Generates the dependency network from Araujo et al. 2011.
#' 
#' Models and conducts network pruning for a species dependency network
#' calculated as pairwise conditional probabilities.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x A matrix of species abundances with species in columns.
#' @param zero.na Automatically make NA values zero.
#' @param prune Reduce the size of the network using a confidence interval
#' test.
#' @param diag.zero Set the matrix diagonal (i.e. the trace) to zero.
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
#' function (x = "species in cols", zero.na = TRUE, prune = TRUE, 
#'     diag.zero = TRUE, pos = TRUE) 
#' {
#'     out <- matrix(NA, nrow = ncol(x), ncol = ncol(x))
#'     for (i in 1:ncol(x)) {
#'         for (j in 1:ncol(x)) {
#'             if (pos) {
#'                 out[i, j] <- calcDepend(x[, i], x[, j])
#'             }
#'             else {
#'                 out[i, j] <- negDepend(x[, i], x[, j])
#'             }
#'         }
#'     }
#'     if (prune) {
#'         out.rm <- co.net(x, diag.zero = diag.zero)
#'         out[out.rm == 0] <- 0
#'     }
#'     else {
#'     }
#'     if (diag.zero) {
#'         diag(out) <- 0
#'     }
#'     rownames(out) <- colnames(out) <- colnames(x)
#'     if (zero.na) {
#'         out[is.na(out)] <- 0
#'     }
#'     return(out)
#'   }
#' 
dep.net <-
function(x='species in cols',zero.na=TRUE,prune=TRUE,diag.zero=TRUE){
  out <- matrix(NA,nrow=ncol(x),ncol=ncol(x))
  for (i in 1:ncol(x)){
    for (j in 1:ncol(x)){
        out[i,j] <- calcDepend(x[,i],x[,j])
    }
  }
  if (prune){
    out.rm <- co.net(x,diag.zero=diag.zero)
    out[out.rm==0] <- 0
  }else{}
  if (diag.zero){diag(out) <- 0}
  rownames(out) <- colnames(out) <- colnames(x)
  if (zero.na){out[is.na(out)] <- 0}
  return(out)
}
