bias <- function(alpha, beta, n){
    ## n is a 2x2 matrix of sample sizes
    
    b <- (n[1, 1] * alpha + n[1, 2]) * (n[2, 1] + n[2, 2]) /
        ((n[1, 1] + n[1, 2]) * (n[2, 1] * alpha + n[2, 2]))
    b
}