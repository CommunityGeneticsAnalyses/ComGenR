#' Calcualtes the mean for a set of genotype.
#' 
#' Calcualtes the mean for a set of genotype.
#' 
#' 
#' @param x Community matrix with species in columns.
#' @param g Grouping (i.e. genotype) vector.
#' @note %% ~~further notes~~
#' @author Matthew K. Lau %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
mean.g <- function(x = 'community matrix', g = 'grouping'){
    return(apply(x,2,function(x,g) tapply(x,g,mean),g=g))
}
