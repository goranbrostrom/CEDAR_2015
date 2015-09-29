run <- function(inmig = TRUE){
    library(sweden14)
    source("R/predict.R")
    if (inmig) {
        
    }
    fem <- getPop("females", years = 2014)
    mal <- getPop("males", years = 2014)
    births <- c(rep(0, 14), getBirths(years = 2014), rep(0, 51))
    fert <- births / fem
    m.deaths <- getDeaths("males", years = 2014)
    f.deaths <- getDeaths("males", year = 2014)
    pop <- fem + mal
    f.mort <- f.deaths / fem
    m.mort <- f.deaths / mal
    x <- predict(mal, fem, fert, m.mort, f.mort)
    x
}