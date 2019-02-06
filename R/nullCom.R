#' Generate a set of null communities based on functions in the vegan package.
#' 
#' This function produces a set of permuted matrices based on an input species
#' abundance or presence-absence matrix.
#' 
#' Provides high level access to the oecosimu function in vegan.
#' 
#' @param com Community matrix with species abundances or presence-absences.
#' Values are converted to presence-absence regardless of input.
#' @param method Permutation method. Options include: r00 = fully random, r0 =
#' maintain site frequencies, r1 = maintain both site and species frequencies.
#' For more options see the oecosimu function in the vegan package.
#' @param nits Number of iterations to conduct.
#' @param burn The number permutations to conduct prior to recording matrices.
#' @param thin The number of discarded matrices between iterations.
#' @return Returns a set of permuted matrices to be used for null modeling. The
#' default null model is the most constrained version (i.e. both species and
#' observation marginal totals are restricted).
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
#' function (com, method = "r1", nits = 5000, burn = 500, thin = 10) 
#' {
#'     com[com != 0] <- 1
#'     for (i in 1:burn) {
#'         post.burn <- commsimulator(x = com, method = method, 
#'             thin = thin)
#'     }
#'     out <- list()
#'     for (i in 1:nits) {
#'         out[[i]] <- commsimulator(x = post.burn, method = method, 
#'             thin = thin)
#'     }
#'     return(out)
#'   }
#' 
nullCom <-
function(com,method='r1',nits=5000,burn=500,thin=10){
                                        #force binary
  com[com!=0] <- 1
  for (i in 1:burn){post.burn <- commsimulator(x=com,method=method,thin=thin)}
  out <- list()
  for (i in 1:nits){out[[i]] <- commsimulator(x=post.burn,method=method,thin=thin)}
  return(out)
}
