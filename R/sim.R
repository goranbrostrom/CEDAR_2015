sim <- function(alpha = 1, beta = 1,
                n = matrix(c(1000, 2000, 2000, 1000), ncol = 2)){
    set.seed(101121)
    ## n :
    ##         Male | female
    ##     -------------
    ## lower :  n11 | n12
    ## upper :  n21 | n22

    ## T :
    ##         Male | female
    ##         -------------
    ## lower :  T11 | T12
    ## upper :  T21 | T22

    ## Model:
    ##         Male       | female
    ##         ------------------------------
    ## lower :  1         | exp(alpha)
    ## upper :  exp(beta) | exp(alpha + beta)

    t11 <- rexp(n[1, 1], 1)
    t12 <- rexp(n[1, 2], exp(alpha))
    t21 <- rexp(n[2, 1], exp(beta))
    t22 <- rexp(n[2, 2], exp(alpha + beta))
    

    dat <- data.frame(enter = numeric(sum(n)),
                      exit = 20 * c(t11, t12, t21, t22),
                      event = rep(1, sum(n)),
                      sex = c(rep(0, n[1, 1]),
                      rep(1, n[1, 2]), rep(0, n[2, 1]),
                      rep(1, n[2, 2])),
                      civ = c(rep(0, n[1, 1]),
                      rep(0, n[1, 2]), rep(1, n[2, 1]),
                      rep(1, n[2, 2]))
                      )
    dat
}
