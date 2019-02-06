#' Network distance for a set of networks.
#' 
#' Takes a list of networks and returns a distance matrix using Euclidean
#' distance.
#' 
#' This function computes the Euclidean (i.e. straight line) distance using all
#' edges for all pairs of networks in a list of networks. The mathematics of
#' using Euclidean distance to summarize the difference between two networks
#' has not been explored fully, so use with caution.
#' 
#' @param dn.t A list of networks to be used for calculating distances.
#' @return Returns a matrix of Euclidean distances for all pairs of networks in
#' the list.
#' @note %% ~~further notes~~
#' @author Matthew K. Lau
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
netDist <- function(dn.t){
    net.d <- matrix(0,nrow=length(dn.t),ncol=length(dn.t))
    rownames(net.d) <- colnames(net.d) <- names(dn.t)
    for (i in 1:nrow(net.d)){
        for (j in 1:ncol(net.d)){
            net.d[i,j] <- sum(abs(dn.t[[i]]-dn.t[[j]])^2)
        }
    }
    net.d <- as.dist(net.d)
    return(net.d)
}
