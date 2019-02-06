#' Community Genetics simulator using the methods of Shuster et al 2006.
#' 
#' Function for simulating the community level effects of genetic variation in
#' a clonal foundation species.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param trees Matrix of phenotypic values for a set of trees.
#' @param insects Matrix of bi-allelic loci for arthropod species.
#' @param z Selection strength.
#' @param VeT tree trait variance
#' @param Ve insect environmental variance
#' @param VeN interaction environmental variance
#' @param K insect carrying capacity
#' @param k.asym Logical: Should default K be asymptotic?
#' @param k.min Minimum value for K.
#' @param k.max Maximum value for K
#' @param cf Correction factor numerical error in generating species abundances
#' at equilibrium.
#' @param artpop.only FALSE
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
#' function (tree.pheno = "tree phenotype matrix", insect = "community phenotype matrix", 
#'     reps = 10, GG = 8, YY = 5, VeT = 8, Ve = 0.1, VeN = 15, K = 100) 
#' {
#'     if (any(tree.pheno == "tree phenotype matrix")) {
#'         tree.pheno <- gpmTrees()
#'     }
#'     if (any(insect == "community phenotype matrix")) {
#'         insect <- gpmCom()
#'     }
#'     T <- nrow(tree.pheno)
#'     I <- nrow(insect)
#'     art_g <- matrix(NA, nrow = T, ncol = I)
#'     art_z <- matrix(NA, nrow = T, ncol = I)
#'     art_Vg <- matrix(NA, nrow = T, ncol = I)
#'     art_Vz <- matrix(NA, nrow = T, ncol = I)
#'     gen_load <- matrix(NA, nrow = T, ncol = I)
#'     dem_load <- matrix(NA, nrow = T, ncol = I)
#'     art_pop <- matrix(NA, nrow = T, ncol = I)
#'     out <- gg.list <- yy.list <- list()
#'     tic <- Sys.time()
#'     for (RR in 1:reps) {
#'         scores_XX <- matrix(NA, nrow = nrow(tree.pheno), ncol = GG)
#'         scores_XX[, 1] <- tree.pheno[, 2] + runif(T, 0, 1) * 
#'             VeT - VeT/2
#'         for (z in 2:GG) {
#'             scores_XX[1:T, z] <- scores_XX[1:T, 1]
#'         }
#'         trees <- scores_XX
#'         for (y in 1:YY) {
#'             for (z in 1:GG) {
#'                 for (i in 1:T) {
#'                   for (j in 1:I) {
#'                     if (trees[i, z] < 2 * insect[j, 1]) {
#'                       art_g[i, j] <- 0
#'                     }
#'                     else if (trees[i, z] > 2 * insect[j, 2]) {
#'                       art_g[i, j] <- 1
#'                     }
#'                     else {
#'                       art_g[i, j] <- (trees[i, z] - 2 * insect[j, 
#'                         1])/(2 * insect[j, 2] - 2 * insect[j, 
#'                         1])
#'                     }
#'                     art_z[i, j] <- 2 * insect[j, 2] * art_g[i, 
#'                       j]^2 + 2 * art_g[i, j] * (1 - art_g[i, 
#'                       j]) * (insect[j, 1] + insect[j, 2]) + 2 * 
#'                       insect[j, 1] * (1 - art_g[i, j])^2 + runif(1) * 
#'                       Ve - Ve/2
#'                     art_Vg[i, j] <- 2 * art_g[i, j] * (1 - art_g[i, 
#'                       j])
#'                     art_Vz[i, j] <- art_Vg[i, j] * (insect[j, 
#'                       2] - insect[j, 1])^2 + Ve
#'                     gen_load[i, j] <- 0.5 * (7.924e-05 * 2.511886^(z - 
#'                       1)) * (art_z[i, j] - trees[i, z])^2
#'                     dem_load[i, j] <- 0.5 * (7.924e-05 * 2.511886^(z - 
#'                       1)) * (art_Vz[i, j])
#'                     art_pop[i, j] <- K * (1 - gen_load[i, j] - 
#'                       dem_load[i, j]) + runif(1) * VeN * (y - 
#'                       1) - VeN * (y - 1)/2
#'                     if (art_pop[i, j] < 0) {
#'                       art_pop[i, j] <- runif(1) * 3
#'                     }
#'                     else {
#'                       art_pop[i, j]
#'                     }
#'                   }
#'                 }
#'                 print(paste(RR, y, z, sep = " "))
#'                 gg.list[[z]] <- art_pop
#'                 names(gg.list)[z] <- paste(RR, y, z, sep = "_")
#'             }
#'             yy.list[[y]] <- gg.list
#'         }
#'         toc <- Sys.time()
#'         out[[RR]] <- yy.list
#'     }
#'     return(out)
#'   }
#' 
cgSim <- function(trees='simulated trees',insects='simulated insects',
                  z='selection strength',VeT='tree trait variance',
                  Ve='insect environmental variance',VeN='interaction environmental variance',
                  K='insect carrying capacity',k.asym=FALSE,k.min=5,k.max=100,
                  cf='correction factor',artpop.only=FALSE){

###Default Parameter Settings
  ##environmental variance in tree trait influences tree heritability (2 for high H2 and 8 for low H2)
  if (any(VeT=='tree trait variance')){VeT <- 2}
  ##selection strength = c from gamma^c
  if (any(z=='selection strength')){z <- 0}
  ##environmental variance for insect trait
  if (any(Ve=='insect environmental variance')){Ve <- 0.1}
  ##step size for environmental variance in interactions (0 15 30 45 60)
  if (any(VeN=='interaction environmental variance')){VeN <- 0}
  ##trees
  if (any(trees=='simulated trees')){
    tree.gpm <- gpmTrees()
    trees <- simTrees(tree.gpm,VeT=VeT)
  }
  if (any(insects=='simulated insects')){
    insects <- simSpp()
  }
  ##insect pop carrying capacity
  if (any(K=='insect carrying capacity')){
    if(k.asym){
      p.k <- 1-ppois(1:nrow(insects),5)
      k.vector <- (p.k*k.max) + ((1-p.k)*k.min)
      K <- round(k.vector,0)
    }else{
      K <- rep(100,nrow(insects))
    }
  }
  ##negative abundance correction factor
  if (any(cf=='correction factor')){cf <- 0}

###Initiate output objects
T <- length(trees) #number of trees
I <- nrow(insects) #number of insects
art_g <- matrix(NA,nrow=T,ncol=I) #insect gene frequency
art_z <- matrix(NA,nrow=T,ncol=I) #insect trait mean
art_Vg <- matrix(NA,nrow=T,ncol=I) #insect gene variability
art_Vz <- matrix(NA,nrow=T,ncol=I) #insect trait variability
gen_load <- matrix(NA,nrow=T,ncol=I) #insect genetic load
dem_load <- matrix(NA,nrow=T,ncol=I) #insect demographic load
En <- matrix(NA,nrow=T,ncol=I) #Environmental effect
#gamma = selection intensity
art_pop <- matrix(NA,nrow=T,ncol=I) #insect population community matrix
                                        #names the species
colnames(art_pop) <- paste('S',I(1:I),sep='')
colnames(art_g) <- colnames(art_z) <- paste('S',I(1:I),sep='')
colnames(art_Vg) <- colnames(art_Vz) <- paste('S',I(1:I),sep='')
colnames(gen_load) <- colnames(dem_load) <- paste('S',I(1:I),sep='')
colnames(En) <- paste('S',I(1:I),sep='')

for (i in 1:T){
                                        #insects on trees
                                        #for each tree i
  for (j in 1:I){
                                        #for each insect j
###Equation 6 from MS - Arthropod (art) Gene frequency (art_g = pij)
    if (trees[i] < 2*insects[j,1]){
      art_g[i,j] <- 0
    }else if (trees[i] > 2*insects[j,2]){
      art_g[i,j] <- 1
    }else{
      art_g[i,j] <- (trees[i] - 2*insects[j,1]) / (2*insects[j,2] - 2*insects[j,1])
    }
###Equation 6 from MS - calculating mean trait Z from art gene frequency
###CHANGE = actually Equation 4
###This is the trait value at equilibrium (zbar^star_ij)
    art_z[i,j] <- 2*insects[j,2]*art_g[i,j]^2 + 
      2*art_g[i,j]*(1-art_g[i,j])*(insects[j,1]+insects[j,2]) + 
        2*insects[j,1]*(1-art_g[i,j])^2 + runif(1)*Ve - Ve/2
    ##Note: the final term (runif(1)*Ve - Ve/2) is the from the e_z term in Eq. 3.

###Art genetic (Vg = sigma^2_Gij) and trait variance (Vz = sigma^2_zbar_ij)
    art_Vg[i,j] <- 2*art_g[i,j]*(1-art_g[i,j])
    art_Vz[i,j] <- art_Vg[i,j]*(insects[j,2] - insects[j,1])^2 + Ve

###Evolutionary (gen) and demographic (dem) loads from selection
### dem_load[i,j] <- 0.5*(selection intensity)*(arthropod trait variance)
### gen_load = 0.5*(selection intensity)*(arthropod mean trait value - tree trait value)^2
    dem_load[i,j] <- 0.5*(0.00007924*2.511886^(z-1))*(art_Vz[i,j])
    gen_load[i,j] <- 0.5*(0.00007924*2.511886^(z-1))*(art_z[i,j] - trees[i])^2
    gamma <- (0.00007924*2.511886^(z-1))
###Equation 7 from MS - art predicted population size as a function of loads and ecological variance
###art_pop = n^star_ij = population of species j and tree i at equilibrium
###art_pop = (K = carrying capacity) * (community selection) + (E_n_ij = environmental variance)
    En[i,j] <- (runif(1)*VeN-VeN/2)
    art_pop[i,j] <- K[j] * (1 - gen_load[i,j] - dem_load[i,j]) + En[i,j]
###preventing art pops from going negative
###NOTE: this code was changed from the original which made 
###zero or negative species into runif(1)*3)
    if (art_pop[i,j] < 0){art_pop[i,j] <- cf}
  } #end insect loop (j)
} #end tree loop (i)

  if (artpop.only){return(art_pop)}else{
    return(list(
                art_pop=art_pop,tree.gpm=tree.gpm,trees=trees,insects=insects,
                art_g=art_g,art_Vg=art_Vg,art_Vz=art_Vz,gamma=gamma,
                dem_load=dem_load,gen_load=gen_load,En=En
                )
           )
  }
}
