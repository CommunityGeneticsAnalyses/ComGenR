#' Co-occurrence Dependency Networks
#' 
#' Based on Aurajo et al. 2011
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x Co-occurrence matrix.
#' @param alpha Significance level. 
#' @param thresh Threshold for inclusion
#' @param zero.na Make NA values equal to 0.
#'
#' @return A matrix representation of the co-occurrence network.
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
#' 
#' 


cdNet <- function(x,alpha=0.05,thresh=1,zero.na=TRUE){
    x <- as.matrix(x)
    x[x < thresh] <- 0
    x[x >= thresh] <- 1
    Na <- diag(t(x) %*% x)
    A <- nrow(x)
    Pa <- Na / A
    Pa <- matrix(Pa,ncol=1)
    Pa.b <- Pa %*% t(Pa)
    Paob <- Pa %*% (1 - t(Pa)) + (1 - Pa) %*% t(Pa)
    NPa.b <- A * Pa.b
    NPaob <- A * Paob
    Va.b <- A * Pa.b * (1 - Pa.b)
    Vaob <- A * Paob * (1 - Paob)
    t <- qt(I(1 - alpha/2),I(A-1))
    d <- vegdist(t(x))
    d <- as.matrix(d)
    ci.u <- NPa.b + t * sqrt(Va.b) ## ;ci.l <- NPa.b - t * sqrt(Va.b)
    ci.l <- NPaob - t * sqrt(Vaob) ## ;ci.u <- NPaob + t * sqrt(Vaob);
    d[NPa.b > ci.u] <- 0;d[NPaob < ci.l] <- 0
    ## d[NPaob > ci.u] <- 0;d[NPaob < ci.l] <- 0
    
    if (zero.na){d[is.na(d)] <- 0}
    return(d)
}
