#' Calculate the standard error for a set of genotypes.
#' 
#' Calculate the standard error for a set of genotypes.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x A community matrix with species in columns.
#' @param g Grouping variable (i.e. genotypes).
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
#' function (x = "community matrix", g = "grouping") 
#' {
#'     return(apply(x, 2, function(x, g) tapply(x, g, function(x) sd(x)/sqrt(length(x))), 
#'         g = g))
#'   }
#' 
se.g <-
function(x='community matrix',g='grouping'){
  return(apply(x,2,function(x,g) tapply(x,g,function(x) sd(x)/sqrt(length(x))),g=g))
}
