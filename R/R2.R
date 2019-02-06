#' Calculates the R-squared coefficient from a range of model outputs.
#' 
#' Calculates the R-squared coefficient from a range of model outputs.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x Model object: ANOVA, REML, adonis2 or dbrda. 
#' @return Returns the R-square value.
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

R2 <- function(x = "aov, reml, adonis2 or dbrda object"){
    if (!(class(x)[1] %in% c("anova.cca", "aov", "dbrda", "lmerMod"))){
        warning("Unknown object.")
        stop()
    }
    if (class(x)[1] == "anova.cca"){
        tab <- as.matrix(x)
        r2 <- tab[1, "SumOfSqs"] / sum(tab[1:(nrow(tab) - 1), "SumOfSqs"])
    }else if (class(x)[1] == "aov"){
        tab <- as.matrix(anova(x))
        r2 <- tab[1, "Sum Sq"] / (tab[1, "Sum Sq"] + tab[2, "Sum Sq"])
    }
    if (class(x)[1] == "dbrda"){
        r2 <- vegan::RsquareAdj(x)$r.squared
    }else if (class(x)[1] == "lmerMod"){
        r2 <- MuMIn::r.squaredGLMM(x)[, "R2c"]
    }
    return(r2)
}
