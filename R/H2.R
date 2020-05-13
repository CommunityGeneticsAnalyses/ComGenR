#' Calculates heritability metrics from a range of model output.
#' 
#' Calculates heritability metrics from a range of model output.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x Model object: ANOVA, REML, adonis2 or dbrda. 
#' @param g A vector of genotypes for use with ANOVA and REML.
#' @param perm Number of permutations to use for reml.
#' @return Returns a heritability score. 
#' @export H2
#' @note %% ~~further notes~~
#' @author Matthew K. Lau
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2


H2 <- function(x = "aov, reml, adonis2 or dbrda object", 
               g = "genotype vector", perm = 10000){
    if (!(class(x)[1] %in% c("anova.cca", "aov", "dbrda", "lmerMod"))){
        warning("Unknown object.")
        stop()
    }
    if (class(x)[1] == "anova.cca"){
        if (any(g == "genotype vector")){
            warning("Please supply a genotype vector.")
            stop()
        }
        tab <- as.matrix(x)
        MSa <- tab[1, "SumOfSqs"] / tab[1, "Df"]
        MSw <- tab[2, "SumOfSqs"] / tab[2, "Df"]
    }else if (class(x)[1] == "aov"){
        if (any(g == "genotype vector")){
            warning("Please supply a genotype vector.")
            stop()
        }
        g <- x$model[,2]
        tab <- as.matrix(anova(x))
        MSa <- tab[1, "Mean Sq"]
        MSw <- tab[2, "Mean Sq"]
    }
                                        # Adjust MS for sample size
    if (class(x)[1] == "anova.cca" | class(x)[1] == "aov"){
        if (all(duplicated(table(g)))){
            k <- table(g)[1]
        }else{
            S <- length(unique(g))
            n. <- length(g)
            ni <- table(g)
            k <- (1/(S-1)) * (n. - (sum(ni^2) / n.))
        }
        s2.a <- (MSa - MSw) / k
        s2.w <- MSw
    }
    if (class(x)[1] == "dbrda"){
        aov.tab <- as.matrix(anova(x, permutations = perm), all = TRUE)
        s2.a <- aov.tab[1, "Variance"]
        s2.w <- aov.tab[2, "Variance"] 
    }else if (class(x)[1] == "lmerMod"){
        s2.a <- as.data.frame((summary(x)[["varcor"]]))[1, "sdcor"]^2
        s2.w <- as.data.frame((summary(x)[["varcor"]]))[2, "sdcor"]^2
    }
                                        # Heritability
    s2.a /  (s2.a + s2.w)
}
