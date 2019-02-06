#' Calculates interaction strength.
#' 
#' Calculates node level statistics describing the strength of a species in the
#' network using the methods of Araujo et al. 2011.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x %% ~~Describe \code{x} here~~
#' @param direction %% ~~Describe \code{direction} here~~
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
#' function (x = "dependency network", direction = "in") 
#' {
#'     if (direction == "in") {
#'         return(apply(x, 2, sum))
#'     }
#'     else {
#'         return(apply(x, 1, sum))
#'     }
#'   }
#' 
coStrength <-
function(x='dependency network',direction='in'){
  if (direction=='in'){
    return(apply(x,2,sum))    
  }else{
    return(apply(x,1,sum))
  }
}
