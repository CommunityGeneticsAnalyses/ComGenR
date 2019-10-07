#' Minimized network function.
#' 
#' Takes a network and community matrix reduces it to species that have
#' connections in the network.
#' 
#' This function is useful mainly for plotting.
#' 
#' @param net A network model in matrix form.
#' @param com A community matrix with species in the same order as the network.
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
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
min.net <- function(net = 'network', com = 'community matrix'){
  out <- list(net=net,com=com)
  if (all(net!='network')){out[[1]] <- net[apply(net,1,sum)>0,apply(net,2,sum)>0]}
  if (all(com!='community matrix')){out[[2]] <- com[,apply(net,2,sum)>0]}
  return(out)
}
