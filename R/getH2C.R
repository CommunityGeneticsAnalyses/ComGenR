#' Calculates the community heritability metric of Shuster et al. 2006.
#' 
#' Conducts the community heritability calculations developed in Shuster et al.
#' 2006 based on a single ordination axis.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x A single NMDS axis or a communiy matrix.
#' @param g A vector of genotypes.
#' @param sibs Sibling level? 1 = clonal.
#' @param method What method? Setting to 'permanova' will return a PERMANOVA
#' based H2C value using the methods of Lamit et al. 2014. Setting to 'nmds'
#' will return an NMDS based H2C value following the methods of Shuster et al.
#' 2006.
#' @param inits Number of permutations to use for the PERMANOVA.
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
#' function (x = "NMDS axis", g = "grouping vector", sibs = "sibling proportion (1=clones)") 
#' {
#'     g <- as.character(g)
#'     aov.tab <- matrix(unlist(summary(aov(x ~ g))), nrow = 2)
#'     if (sibs == "sibling proportion (1=clones)") {
#'         sibs <- 1
#'     }
#'     S <- length(unique(as.character(g)))
#'     ni <- table(g)
#'     n. <- sum(ni)
#'     if (all(ni == ni[1])) {
#'         k <- mean(ni)
#'     }
#'     else {
#'         k <- 1/(S - 1) * (n. - (sum(ni^2)/n.))
#'     }
#'     s2s <- (aov.tab[1, 3] - aov.tab[2, 3])/k
#'     s2w <- aov.tab[2, 3]
#'     s2total <- s2s + s2w
#'     H2C <- s2s/s2total
#'     t <- H2C * (1/sibs)
#'     if (all(ni == ni[1])) {
#'         SE <- ((2 * (((1 - t)^2) * (1 + (k - 1) * t)^2))/(k * 
#'             (k - 1) * (S - 1)))^(1/2)
#'     }
#'     else {
#'         SE <- ((2 * (n. - 1) * ((1 - t)^2) * (1 + (k - 1) * t)^2)/((k^2) * 
#'             (n. - S) * (S - 1)))^(1/2)
#'     }
#'     CI <- SE * 1.96
#'     return(c(H2C = H2C, CI = CI, SE = SE))
#'   }
#' 
getH2C <- function(x,g='grouping vector',sibs='sibling proportion (1=clones)',method='permanova'){
  g <- as.character(factor(g))
  if (method=='nmds'&is.matrix(x)){
    x <- nmds.min(nmds(vegdist(x),1,1))
    x <- x[,1]
  }
  if (method=='nmds'){
    aov.tab <- matrix(unlist(summary(aov(x~g))),nrow=2)
  }else{
    aov.tab <- as.vector(adonis(x~g,permutations=1)$aov.tab)
  }
  if (sibs=='sibling proportion (1=clones)'){sibs <- 1}
                                        #S = number of clones, families, sires, etc.
  S <- length(unique(as.character(g))) 
                                        #ni = number of individuals in ith clone
  ni <- table(g)
                                        #n. = total number of inidividuals in the analysis
  n. <- sum(ni)
                                        #k = ni in expected means squares 
                                        #= ni if design is balanced 
                                        #= kl if design is unbalanced
  if (all(ni==ni[1])){
    k <- mean(ni)
  }else{
    ##this is kl
    print('Note: Unbalanced design.')
    k <- 1/(S-1)*(n.-(sum(ni^2)/n.))
  }
                                        #s = sigma here
                                        #variance among = s2s = (MSs-MSw)/k
                                        #variance within = s2w = MSw
                                        #total variance = Vp = s2total = s2s + s2w
                                        #H2c = community heritability
  s2s <- (aov.tab[1,3]-aov.tab[2,3])/k
  s2w <- aov.tab[2,3]
  s2total <- s2s+s2w
  H2C <- s2s / s2total

###Confidence limits for H2C
  t <- H2C * (1/sibs)
  if (all(ni==ni[1])){
    SE <- ((2*(((1-t)^2)*(1+(k-1)*t)^2))/(k*(k-1)*(S-1)))^(1/2)
  }else{
    ##Unbalanced design with kl
    SE <- ((2*(n.-1)*((1-t)^2)*(1+(k-1)*t)^2)/((k^2)*(n.-S)*(S-1)))^(1/2)
  }
  CI <- SE*1.96
  return(c(lower.CI=(H2C-CI),H2C=H2C,upper.CI=(H2C+CI)))
}
