#' Tree simulator.
#' 
#' Generates a matrix of phenotypic values for a simulated forest of trees.
#' 
#' 
#' @param tree.gpm A genotype-phenotyp mapping matrix. See gpmTrees.
#' @param VeT Tree trait environmental variance.
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
simTrees <- function(tree.gpm='tree genotype-phenotype map',VeT=2){
  if (length(tree.gpm)==1){tree.gpm <- gpmTrees()}
  T <- nrow(tree.gpm) #number of trees
  trees <- tree.gpm[,2] + runif(T,0,1) * VeT - VeT/2 #scores are in second column of trees
  return(trees)
}
