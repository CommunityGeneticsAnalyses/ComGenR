#' Relativize a matrix of values.
#' 
#' Takes a matrix and relativizes by the maxiumum or total of each column.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x A matrix.
#' @param rel.type Determines whether the matrix will be relativized
#' by "max" (maximum) or "sum" (total) of each column.
#' @return Returns the relativzed matrix.
#' @export rel
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
rel <- function(x, rel.type = "max"){
    if (rel.type == "max"){
        apply(x , 2, function(x) x / max(x))
    }else if (rel.type == "sum"){
        apply(x , 2, function(x) x / sum(x))
    }else{
        warning("Unknown relativization type.")
    }
}
