#' Community genotype-phenotype mapping function.
#' 
#' Creates a set of phenotypes based on genotypic values for a community of
#' organisms (e.g. insects) based on the methods of Shuster et al. 2006. This
#' is a support function for the cgSim function.
#' 
#' This function provides a simple way to populate a set of phenotypic values
#' for a model community to be used in a simulation of community genetics
#' effects.
#' 
#' @param n Number of species.
#' @param het.values Range of heterozygote values.
#' @param allelic.range Range of allelic values.
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' @note %% ~~further notes~~
#' @author Matthew K. Lau %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 

gpmCom <- function(n = 'number of species', het.values = c(5,21), allelic.range = c(0,3)){
    if (n=='number of species'){n <- 25}
                                        #generate heterozygous alleles for n species
    com <- matrix(NA,nrow=n,ncol=2)
    com[,1] <- runif(n,het.values[1],het.values[2]) #heterozygote value between 5 and 21
    com[,2] <- runif(n,allelic.range[1],allelic.range[2]) #range between 0 and 3
                                        #map genotype to phenotype
    com. <- com
    com.[,1] <- (com[,1]-0.5*com[,2])/2 #C allelic value = (HET - 0.5*range)/2
    com.[,2] <- (com[,1]+0.5*com[,2])/2 #D allelic value = (HET + 0.5*range)/2

    return(com.)

}
