#' Pruning function for the Araujo et al. 2011 method.
#' 
#' Defines whether or not a node should be included in a network using the
#' analytical methods of Araujo et al. 2011.
#' 
#' 
#' @param a Species "a".
#' @param b Species "b".
#' @param std Logical: should the values be standardized?
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' @export null.prune
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
null.prune <- function(a,b,std=TRUE){

###Method for producing network models from co-occurrence data.
###Developed in Araujo et al. 2011 Ecography Using species co-occurrence networks to assess the impacts of climate change
###Coded 18 Sep 2013 MKLau

## E is an environmental lattice with rows = N and cols = M and has size A = N*M.
## A = total number of sites
## Na = number of sites with species a
                                        #determine number of sites
A <- length(a)
                                        #NULL probabilities
## Assuming no interactions among species
## Probability of observing species a
## Pa <- Na/A
## Probability of observing both species a and b simultaneously
## Pa&b = Pa * Pb
## Probability of observing either species a and b
## Pa+b = Pa(1-Pb) + (1-Pa)Pb = Pa + Pb - 2PaPb 
## Probability of observing neither species a nor b
## P!a&!b = (1-Pb)(1-Pa) = 1 - Pa - Pb + PaPb
## and it follows that 
## Pa&b + Pa+b + P!a&!b = 1

pa <- sum(a)/A
pb <- sum(b)/A
paANDb <- pa * pb
paORb <- pa + pb - 2*pa*pb
pNOTaORb <- 1- pa - pb + pa*pb

## Three states, i = ab, ii a+b = (a&!b + !a&b), iii = !a!b
                                        #expected frequencies
## E(i) = APa&b
## E(ii) = APa+b
## E(iii) = AP!a&!b

Ei <- A*paANDb
Eii <- A*paORb
Eiii <- A*pNOTaORb
                                        #variance of frequencies
## Var(i) = APa&b(1-Pa&b)
## Var(ii) = APa+b(1-APa+b)
## Var(iii) = AP!a&!b(1-AP!a&!b)

Vi <- A*paANDb*(1-paANDb)
Vii <- A*paORb*(1-paORb)
Viii <- A*pNOTaORb*(1-pNOTaORb)
                                        
## Critical interval
## Critical threshold (+ = attraction and - = repulstion)
## CI.u = E(i) + 2*Var(i)^2 = attractive overlap
## CI.l = E(i) - 2*Var(i)^2 = repulsive overlap

ci.u <- Ei + 2*Vi^(1/2)
ci.l <- Ei - 2*Vi^(1/2)
                                        #Oi = observed a AND b
Oi <- length(a[(a+b)==2])
                                        #Determine output (Oi = significant, 0 = not)
if (Oi > ci.u | Oi < ci.l){
  if (std){
    out <- (Oi - Ei) / sqrt(Vi)
  }else{
    out <- Oi
  }

}else{
  out <- 0
}
return(out)
}
