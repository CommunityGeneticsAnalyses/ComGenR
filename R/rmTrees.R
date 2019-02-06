#' Tree removal simulator.
#' 
#' Removes trees from a matrix under three scenarios.
#' 
#' Removes trees from a simulation to simulate selection. Three
#' options for removal: random, degree and (geno- or pheno-)type
#' 
#' 
#' @param x A network in matrix representation.
#' @param nits Number of iterations to be used. 
#' @param method Method to be used: 'random','degree','type'.
#' @param type Type of removel.
#' @param return.nits LOGICAL: should all iterations be returned. 
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
rmTrees <- function(x = 'network', nits = 100, method = c('random', 'degree', 'type'), type = 'grouping', return.nits = FALSE){
    out.nits <- list()
    for (i in 1:nits){
        if (method[1]=='random'){
            rm.x <- x
            rm.x[rm.x!=0] <- 1
            itr <- 0
            spp.deg <- apply(rm.x,2,sum)
            live.trees <- sign(apply(rm.x,1,sum))
            rm.prob <- live.trees/length(live.trees[live.trees!=0])
            while(all(spp.deg!=0)){
                live.trees <- sign(apply(rm.x,1,sum))
                rm.prob <- live.trees/length(live.trees[live.trees!=0])
                rm.tree <- sample((1:nrow(rm.x)),1,prob=rm.prob)
                rm.x[rm.tree,] <- 0
                spp.deg <- apply(rm.x,2,sum)
                itr <- itr + 1
            }
            out <- (itr/nrow(x))
        }else if (method[1]=='degree'){
            rm.x <- x
            rm.x[rm.x!=0] <- 1
            itr <- 0
            spp.deg <- apply(rm.x,2,sum)
            deg.trees <- apply(rm.x,1,sum)
            rm.prob <- deg.trees/max(deg.trees)
            while(all(spp.deg!=0)){
                deg.trees <- apply(rm.x,1,sum)
                rm.prob <- deg.trees/max(deg.trees)
                rm.tree <- sample((1:nrow(rm.x)),1,prob=rm.prob)
                rm.x[rm.tree,] <- 0
                spp.deg <- apply(rm.x,2,sum)
                itr <- itr + 1
            }
            out <- (itr/nrow(x))
        }else if (method[1]=='type'){
            if (any(type=='grouping')){warning('Please provide a grouping vector.')}
            rm.x <- x
            live.trees <- rep(1,length(type))
            itr <- 0
            spp.deg <- apply(sign(x),2,sum)
            rm.type <- sample(unique(type[live.trees==1]),1)
            type.d <- (type - rm.type)^2
            rm.p <- 1 - (type.d/max(type.d))
            rm.p[rm.p==0] <- min(rm.p[rm.p!=0])/1000
            while(all(spp.deg!=0)&any(live.trees==1)){
                if (sum(live.trees)==1){rm.tree <- (1:length(type))[live.trees==1]}else{
                    rm.p <- rm.p/max(rm.p[live.trees==1])
                    rm.tree <- sample((1:length(type))[live.trees==1],1,prob=rm.p[live.trees==1])
                }
                                        #make that tree's community zero
                rm.x[rm.tree,] <- 0
                itr <- itr + 1
                spp.deg <- apply(rm.x,2,sum)
                live.trees[rm.tree] <- 0
            }
            out <- (itr/nrow(x))
        }else{warning('Unknown simulation method.')}
        out.nits[[i]] <- out
    }
    if (return.nits){out <- unlist(out.nits)}else{out <- mean(unlist(out.nits))}
    return(out)
}
