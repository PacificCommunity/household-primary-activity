# Load setup files
source("setup.R")
source("households.R")

#### ****************************** Livestock processing ******************************* ####

household <- households(hhld)
livestock <- livestock_hhld(hhld)


livestock_pig <- function(hhld){
  livestock_pig_nat <- pActivity %>%
    filter(livestock_pig == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'LVKPIG',
           UNIT_MEASURE = 'N'
    )
  
  livestock_pig_str <- pActivity %>%
    filter(livestock_pig == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'LVKPIG',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(livestock_pig_nat, livestock_pig_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
}

livestock_chicken <- function(hhld){
  livestock_chicken_nat <- pActivity %>%
    filter(livestock_chicken == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'LVKCHK',
           UNIT_MEASURE = 'N'
    )
  
  livestock_chicken_str <- pActivity %>%
    filter(livestock_chicken == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'LVKCHK',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(livestock_chicken_nat, livestock_chicken_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
  
}


livestock_duck <- function(hhld){
  livestock_duck_nat <- pActivity %>%
    filter(livestock_duck == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'LVKDCK',
           UNIT_MEASURE = 'N'
    )
  
  livestock_duck_str <- pActivity %>%
    filter(livestock_duck == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'LVKDCK',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(livestock_duck_nat, livestock_duck_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
  
}

livestock_other <- function(hhld){
  livestock_other_nat <- pActivity %>%
    filter(livestock_other == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'LVKOTH',
           UNIT_MEASURE = 'N'
    )
  
  livestock_other_str <- pActivity %>%
    filter(livestock_other == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'LVKOTH',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(livestock_other_nat, livestock_other_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
  
}

