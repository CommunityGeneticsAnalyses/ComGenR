#' Calculates interaction strength.
#' 
#' Calculates node level statistics describing the strength of a species in the
#' network using the methods of Araujo et al. 2011.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x Dependency network.
#' @param direction In or out direction for interaction.
#' @return Generates a vector of relative interaction strengths.
#' @export coStrength
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

coStrength <- function(x = 'dependency network', direction = 'in'){ 
    if (direction=='in'){ 
        return(apply(x,2,sum)) 
    }else{
        return(apply(x,1,sum)) 
    } 
}
