### Co-occurrence Dependency Networks
### Based on Aurajo et al. 2011

cdNet <- function(x,alpha=0.05,thresh=1){
    x <- as.matrix(x)
    x[x < thresh] <- 0
    x[x >= thresh] <- 1
    Na <- diag(t(x) %*% x)
    A <- nrow(x)
    Pa <- Na / A
    Pa <- matrix(Pa,ncol=1)
    Pa.b <- Pa %*% t(Pa)
    NPa.b <- A * Pa.b
    Va.b <- A * Pa.b * (1 - Pa.b)
    t <- qt(I(1 - alpha/2),I(A-1))
    ci.u <- NPa.b + t * sqrt(Va.b)
    ci.l <- NPa.b - t * sqrt(Va.b)
    d <- vegdist(t(x))
    d <- as.matrix(d)
    d[Npa.b > ci.u] <- 0
    d[Npa.b < ci.l] <- 0
    return(d)
}
