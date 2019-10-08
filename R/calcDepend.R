#' Calculate species dependencies (i.e. conditional probabilities).
#' 
#' Calculates the species dependencies for a given set of species abundances as
#' the conditional probability of two species.
#' 
#' 
#' @param a The abundance of species "a".
#' @param b The abundance of species "b".
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' @export calcDepend
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
#' function (a, b) 
#' {
#'     return(length(a[(a + b) == 2])/sum(a))
#'   }
#' 
calcDepend <-
function(a,b){
  return(length(a[(a+b)==2])/sum(a)) #intersection of a with b divided by the total of a
}
