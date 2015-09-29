predict <- function(males, females,
                    female.fert,
                    male.mort,
                    female.mort,
                    male.inmigr = 0,
                    female.inmigr = 0,
                    random = TRUE){

    ## Note that 'female.fert' must be full-length (0...100)!!
    
    ## This function predicts
    ## population size by age at the end of the given year,
    ## or, at Jan. 1 the year 'in.year + 1'.

    ## pop is the sum of 'males' and 'females'.

    ## fertility and mortality are given as age-specific rates, ages 0-100.
    
    ## inmig is assumed to be zero by default. Should be given as
    ## absolute numbers.

    ## Update:
    ## (needs some more sophistication! later....

    if (random){# randomly
        male.deaths <- rpois(length(male.mort), male.mort * males)
        female.deaths <- rpois(length(female.mort), female.mort * females)
        
        males.out <- males - male.deaths
        females.out <- females - female.deaths
        males.out <- males.out + male.inmigr
        females.out <- females.out + female.inmigr
        births <- sum(rpois(length(female.fert), female.fert * females))
        male.births <- 0.514 * births
        female.births <- (1 - 0.514) * births
        males.out <- c(male.births, males.out)
        females.out <- c(female.births, females.out)
        n <- length(males.out)
        males.out[n-1] <- sum(males.out[(n-1):n])
        females.out[n-1] <- sum(females.out[(n-1):n]) 
        males.out <- males.out[-n]
        females.out <- females.out[-n]

    }else{# non-randomly
        male.deaths <- male.mort * males
        female.deaths <- female.mort * females
        
        males.out <- males - male.deaths
        females.out <- females - female.deaths
        males.out <- males.out + male.inmigr
        females.out <- females.out + female.inmigr
        births <- sum(female.fert * females)
        male.births <- 0.514 * births
        female.births <- (1 - 0.514) * births
        males.out <- c(male.births, males.out)
        females.out <- c(female.births, females.out)
        n <- length(males.out)
        males.out[n-1] <- sum(males.out[(n-1):n])
        females.out[n-1] <- sum(males.out[(n-1):n])
        males.out <- males.out[-n]
        females.out <- females.out[-n]

    } 
    names(males.out) <- 0:100
    names(females.out) <- 0:100
    list(males = males.out, females = females.out)
}    
