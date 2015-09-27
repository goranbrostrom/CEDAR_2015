predict <- function(males, females,
                    female.fert,
                    male.mort,
                    female.mort,
                    inmigr = 0,
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
        males.out <- males.out + inmigr / 2
        females.out <- females.out + inmigr / 2
        male.births <- sum(rpois(length(female.fert), female.fert * females)) / 2
        female.births <- male.births # sic!
        males.out <- c(male.births, males.out)
        females.out <- c(female.births, females.out)
        
        males.out <- males.out[-length(males.out)]
        females.out <- females.out[-length(females.out)]

    }else{# non-randomly
        male.deaths <- male.mort * males
        female.deaths <- female.mort * females
        
        males.out <- males - male.deaths
        females.out <- females - female.deaths
        males.out <- males.out + inmigr / 2
        females.out <- females.out + inmigr / 2
        male.births <- sum(female.fert * females) / 2
        female.births <- male.births # sic!
        males.out <- c(male.births, males.out)
        females.out <- c(female.births, females.out)
        
        males.out <- males.out[-length(males.out)]
        females.out <- females.out[-length(females.out)]

    } 
    
    list(males = males.out, females = females.out)
}    
