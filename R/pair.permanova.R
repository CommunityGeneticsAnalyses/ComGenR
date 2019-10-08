#' Conduct pairwise PerMANOVAs.
#' 
#' This function conducts PerMANOVAs for all pairs of levels for a single
#' factor.
#' 
#' This is a high level function built on the adonis function in the vegan
#' package.
#' 
#' @param x Community matrix.
#' @param f Factor to be used.
#' @param nits Number of iterations to be used for each PerMANOVA.
#' @return The function returns a matrix of p-values for the comparisons.
#' @export pair.permanova
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
#' ## The function is currently defined as
#' function (x, f, nits = 999) 
#' {
#'     require(vegan)
#'     f. <- sort(unique(f))
#'     out <- list()
#'     p.out <- array(NA, dim = c(length(f.), length(f.)))
#'     rownames(p.out) <- colnames(p.out) <- f.
#'     h <- 1
#'     for (i in 1:length(f.)) {
#'         k <- i + 1
#'         for (j in k:length(f.)) {
#'             if (i != j & j <= length(f.)) {
#'                 print(paste(f.[i], f.[j], sep = " vs "))
#'                 y <- x[f == f.[i] | f == f.[j], ]
#'                 yf <- factor(f[f == f.[i] | f == f.[j]])
#'                 out[[h]] <- as.matrix(adonis(y ~ yf)$aov.tab, 
#'                   permutations = nits)
#'                 p.out[i, j] <- out[[h]][1, dim(out[[h]])[2]]
#'                 names(out)[h] <- paste(f.[i], f.[j], sep = " vs ")
#'                 h <- h + 1
#'             }
#'             else {
#'             }
#'         }
#'     }
#'     out <- list(f.tables = out, p.mat = p.out)
#'     return(out)
#'   }
#' 
pair.permanova <- function(x,f,nits=999){
  require(vegan)
  f. <- sort(unique(f))
  out <- list()
  p.out <- array(NA,dim=c(length(f.),length(f.)))
  rownames(p.out) <- colnames(p.out) <- f.
  h <- 1
  for (i in 1:length(f.)){
    k <- i + 1
    for (j in k:length(f.)){
      if (i!=j & j<=length(f.)){
        print(paste(f.[i],f.[j],sep=' vs '))
        y <- x[f == f.[i] | f == f.[j],]
        yf <- factor(f[f == f.[i] | f == f.[j]])
        out[[h]] <- as.matrix(adonis(y~yf)$aov.tab,permutations=nits)
        p.out[i,j] <- out[[h]][1,dim(out[[h]])[2]]
        names(out)[h] <- paste(f.[i],f.[j],sep=' vs ')
        h <- h + 1
      }else{}
    }
  }
  out <- list(f.tables=out,p.mat=p.out)
  return(out)
}
