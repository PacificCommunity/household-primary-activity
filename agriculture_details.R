# Load setup files
source("setup.R")
source("households.R")

#### ****************************** Livestock processing ******************************* ####

household <- households(hhld)
agriculture <- agriculture_hhld(hhld)


agriculture_vege <- function(hhld){
  agriculture_vege_nat <- pActivity %>%
    filter(agric_vege == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'ARGVEG',
           UNIT_MEASURE = 'N'
    )
  
  agriculture_vege_str <- pActivity %>%
    filter(agric_vege == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'ARGVEG',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(agriculture_vege_nat, agriculture_vege_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
}

agriculture_tuber <- function(hhld){
  agriculture_tub_nat <- pActivity %>%
    filter(agric_tuber == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'ARGTUB',
           UNIT_MEASURE = 'N'
    )
  
  agriculture_tub_str <- pActivity %>%
    filter(agric_tuber == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'ARGTUB',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(agriculture_tub_nat, agriculture_tub_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
  
}


agriculture_frut <- function(hhld){
  agriculture_frut_nat <- pActivity %>%
    filter(agric_fruit == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'ARGFRT',
           UNIT_MEASURE = 'N'
    )
  
  agriculture_frut_str <- pActivity %>%
    filter(agric_fruit == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'ARGFRT',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(agriculture_frut_nat, agriculture_frut_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
  
}
