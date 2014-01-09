CoNetwork <-
function(x='community matrix'){
###Runs all steps of the process for modeling
###Co-occurrence networks described by Araujo et al. 2011.
###It depends on the seenetR.R script which contains both the
###Araujo functions and related co-occurrence null modeling
###functions.

###Inputs: 
#x = matrix of co-occurrence with species in columns

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
