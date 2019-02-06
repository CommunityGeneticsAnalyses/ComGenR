

netMean <- function(x, zero.na = TRUE){
    x <- Reduce("+", x) / sum(unlist(x))
    if (zero.na){x[is.na(x)] <- 0}
    x
}
