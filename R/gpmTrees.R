#' Foundation tree species genotype-phenotype mapping function for cgSim.
#' 
#' Generates a matrix of genotypic and phenotypic values using the methods of
#' Shuster et al. 2006 to be used in the cgSim function.
#' 
#' This function populates a matrix to be used as the foundation tree species
#' that is influencing a set of species in a community in the cgSim function.
#' 
#' @param pheno A vector of phenotypic values for a set of genotypes.
#' @param reps Scalar to set the number of replicates for the genotypes.
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
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
gpmTrees <- function(pheno = 'phenotype of genotypes', reps = 'replication'){
    if (any(pheno == 'phenotype of genotypes')){
        pheno <- c(11.00,12.50,13.75,16.00,14.00,15.25,17.50,16.50,18.75,21.00)
    }
    if (reps=='replication'){reps <- 5}
    trees <- list()
    for (i in 1:length(pheno)){
        trees[[i]] <- rep(pheno[i],reps)
    }
    return(cbind(geno=gl(length(pheno),reps),pheno=unlist(trees)))
}
