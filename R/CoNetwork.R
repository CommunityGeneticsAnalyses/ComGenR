#' Co-Occurrence based network modeling.
#' 
#' Models and plots a network model using the methods of Araujo et al. 2011.
#' 
#' 
#' @param x Community matrix with species in columns.
#' @param threshold Level of abundance necessary for inclusion in the modeling
#' process. This allows for the removal of some species that are at levels of
#' abundance that are not likely to be ecologically relevant.
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
#' function (x, plot.net = TRUE, scalar = 3, min.vsize = 0.1) 
#' {
#'     bc.d <- as.matrix(vegdist(t(x)))
#'     prune <- co.net(x)
#'     bc.d[prune == 0] <- 0
#'     thresh <- percThreshold(bc.d)$threshold
#'     pruned.net <- bc.d
#'     pruned.net[bc.d < thresh] <- 0
#'     if (plot.net) {
#'         v.cex <- apply(x, 2, sum)
#'         v.cex <- (((v.cex/sum(v.cex))/max((v.cex/sum(v.cex)))) * 
#'             scalar) + min.vsize
#'         gplot(abs(pruned.net), displaylabels = TRUE, gmode = "graph", 
#'             pad = 1.5, edge.lwd = (abs(pruned.net)), vertex.cex = v.cex, 
#'             vertex.col = "grey")
#'     }
#'     return(pruned.net)
#'   }
#' 
CoNetwork <-
function(x='community matrix',threshold=0){
###Runs all steps of the process for modeling
###Co-occurrence networks described by Araujo et al. 2011.
###It depends on the seenetR.R script which contains both the
###Araujo functions and related co-occurrence null modeling
###functions.

###Inputs: 
#x = matrix of co-occurrence with species in columns

#Step 0. Remove observations below threshold
x[x<=threshold] <- 0

#Step 1. Calculate a Bray-Curtis distance matrix

bc.d <- as.matrix(vegdist(t(x)))

#Step 2. Prune distance matrix based on co-occurrence probabilities

prune <- co.net(x)
bc.d[prune==0] <- 0

#Step 3. Reduce to percolation threshold
thresh <- percThreshold(bc.d)$threshold
pruned.net <- bc.d
pruned.net[bc.d<thresh] <- 0

return(pruned.net)

}
