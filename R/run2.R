run2 <- function(inmig = TRUE){
    library(sweden14)
    source("R/predict.R")
    if (inmig) {
          male.inm <- getInmig(sex = "males", years = 2014)
          female.inm <- getInmig(sex = "females", years = 2014)
    }
    fem <- getPop("females", years = 2014)
    mal <- getPop("males", years = 2014)
    births <- c(rep(0, 14), getBirths(years = 2014), rep(0, 51))
    fert <- births / fem
    m.deaths <- getDeaths("males", years = 2014)
    f.deaths <- getDeaths("males", years = 2014)
    pop <- fem + mal
    f.mort <- f.deaths / fem
    m.mort <- f.deaths / mal
    x <- predict(mal, fem, fert, m.mort, f.mort, male.inm, female.inm)
    n <- 50
    popa <- numeric(n)
    popa[1] <- sum(x$males + x$females)
    for (i in 2:n){
       x <- predict(x$males, x$females, fert, m.mort, f.mort, male.inm, female.inm)
       popa[i] <- sum(x$males + x$females)
    }
    plot(2015:2064, popa *10^(-6), type = "s", #ylim = c(8, 10), 
         ylab = "Pop (millions)", xlab = "Year", axes = FALSE, col = "blue")
    axis(1, at = c(2015, 2026, 2050, 2065))
    axis(2, at = c(10, 12, 14))
    box()
    #abline(h = c(9, 10), lty = 2)
    popa
}