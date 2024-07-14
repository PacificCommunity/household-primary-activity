#Load the source file

source("setup.R")

#### ********************** Generate the total number of households *************************************************** ####

households <- function(hhld){
    pActivity_households_nat <- pActivity %>%
    select(year, countryCode, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'HHCNT',
           UNIT_MEASURE = 'N'
           )
    
    pActivity_households_nat <- pActivity_households_nat %>%
      group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
      summarise(hhtotal = round(sum(hhwt), 0))
    
    pActivity_households_str <- pActivity %>%
    select(year, strataCode, hhwt) %>%
    mutate(INDICATOR = 'HHCNT',
           UNIT_MEASURE = 'N'
           )
    
    pActivity_households_str <- pActivity_households_str %>%
      group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
      summarise(hhtotal = round(sum(hhwt), 0))
  
  hhld <- rbind(pActivity_households_nat, pActivity_households_str) 
  return(hhld)
}

#Generate Livestock households

livestock_hhld <- function(hhld) {
  pActivity_households_nat <- pActivity %>%
    filter(livestock == 1) %>%
    select(year, countryCode, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'HHLVK',
           UNIT_MEASURE = 'N'
           )
  
  pActivity_households_nat <- pActivity_households_nat %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  pActivity_households_str <- pActivity %>%
    filter(livestock == 1) %>%
    select(year, strataCode, hhwt) %>%
    mutate(INDICATOR = 'HHLVK',
           UNIT_MEASURE = 'N'
           )
  
  pActivity_households_str <- pActivity_households_str %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  hhld <- rbind(pActivity_households_nat, pActivity_households_str) 
  return(hhld)
  
}

#Generate Fisheries households

fisheries_hhld <- function(hhld) {
  pActivity_households_nat <- pActivity %>%
    filter(fisheries == 1) %>%
    select(year, countryCode, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'HHFSH',
           UNIT_MEASURE = 'N'
           )
  
  pActivity_households_nat <- pActivity_households_nat %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  pActivity_households_str <- pActivity %>%
    filter(fisheries == 1) %>%
    select(year, strataCode, hhwt) %>%
    mutate(INDICATOR = 'HHFSH',
           UNIT_MEASURE = 'N'
           )
  
  pActivity_households_str <- pActivity_households_str %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  hhld <- rbind(pActivity_households_nat, pActivity_households_str) 
  return(hhld)
  
}

#Generate Agriculture households

agriculture_hhld <- function(hhld) {
  pActivity_households_nat <- pActivity %>%
    filter(agric == 1) %>%
    select(year, countryCode, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'HHARG',
           UNIT_MEASURE = 'N'
           )
  
  pActivity_households_nat <- pActivity_households_nat %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  pActivity_households_str <- pActivity %>%
    filter(agric == 1) %>%
    select(year, strataCode, hhwt) %>%
    mutate(INDICATOR = 'HHARG',
           UNIT_MEASURE = 'N'
           )
  
  pActivity_households_str <- pActivity_households_str %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  hhld <- rbind(pActivity_households_nat, pActivity_households_str) 
  return(hhld)
  
}