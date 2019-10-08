#' Calculate the standard error for a set of genotypes.
#' 
#' Calculate the standard error for a set of genotypes.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x A community matrix with species in columns.
#' @param g Grouping variable (i.e. genotypes).
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' @export se.g
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
se.g <- function(x = 'community matrix', g = 'grouping'){
    apply(x,2,function(x,g) tapply(x,g,function(x) sd(x)/sqrt(length(x))),g=g)
}
