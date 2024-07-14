# Load setup files
source("setup.R")
source("households.R")

#### ****************************** Livestock processing ******************************* ####

household <- households(hhld)
fisheries <- fisheries_hhld(hhld)


fisheries_inshore <- function(hhld){
  fisheries_inshore_nat <- pActivity %>%
    filter(fishloc_inshore == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'FSHINS',
           UNIT_MEASURE = 'N'
    )
  
  fisheries_inshore_str <- pActivity %>%
    filter(fishloc_inshore == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'FSHINS',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(fisheries_inshore_nat, fisheries_inshore_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
}

fisheries_nearshore <- function(hhld){
  fisheries_nearshore_nat <- pActivity %>%
    filter(fishloc_nearshore == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'FSHNSH',
           UNIT_MEASURE = 'N'
    )
  
  fisheries_nearshore_str <- pActivity %>%
    filter(fishloc_nearshore == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'FSHNSH',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(fisheries_nearshore_nat, fisheries_nearshore_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
  
}


fisheries_offshore <- function(hhld){
  fisheries_offshore_nat <- pActivity %>%
    filter(fishloc_offshore == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'FSHOFS',
           UNIT_MEASURE = 'N'
    )
  
  fisheries_offshore_str <- pActivity %>%
    filter(fishloc_offshore == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'FSHOFS',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(fisheries_offshore_nat, fisheries_offshore_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
  
}


fisheries_otherloc <- function(hhld){
  fisheries_otherloc_nat <- pActivity %>%
    filter(fishloc_other == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'FSHOFS',
           UNIT_MEASURE = 'N'
    )
  
  fisheries_otherloc_str <- pActivity %>%
    filter(fishloc_other == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'FSHOFS',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(fisheries_otherloc_nat, fisheries_otherloc_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
  
}


fisheries_gleaning <- function(hhld){
  fisheries_gleaning_nat <- pActivity %>%
    filter(fishmethod_gleaning == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'FSHGLEN',
           UNIT_MEASURE = 'N'
    )
  
  fisheries_gleaning_str <- pActivity %>%
    filter(fishmethod_gleaning == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'FSHGLEN',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(fisheries_gleaning_nat, fisheries_gleaning_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
  
}


fisheries_line <- function(hhld){
  fisheries_line_nat <- pActivity %>%
    filter(fishmethod_line == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'FSHLINE',
           UNIT_MEASURE = 'N'
    )
  
  fisheries_line_str <- pActivity %>%
    filter(fishmethod_line == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'FSHLINE',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(fisheries_line_nat, fisheries_line_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
  
}


fisheries_net <- function(hhld){
  fisheries_net_nat <- pActivity %>%
    filter(fishmethod_net == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'FSHNETS',
           UNIT_MEASURE = 'N'
    )
  
  fisheries_net_str <- pActivity %>%
    filter(fishmethod_net == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'FSHNETS',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(fisheries_net_nat, fisheries_net_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
  
}


fisheries_spear <- function(hhld){
  fisheries_spear_nat <- pActivity %>%
    filter(fishmethod_spear == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'FSHSPER',
           UNIT_MEASURE = 'N'
    )
  
  fisheries_spear_str <- pActivity %>%
    filter(fishmethod_spear == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'FSHSPER',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(fisheries_spear_nat, fisheries_spear_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
  
}


fisheries_mthOther <- function(hhld){
  fisheries_mthother_nat <- pActivity %>%
    filter(fishmethod_other == 1) %>%
    select(countryCode, year, hhwt) %>%
    rename(strataCode = countryCode) %>%
    mutate(INDICATOR = 'FSHOTHR',
           UNIT_MEASURE = 'N'
    )
  
  fisheries_mthother_str <- pActivity %>%
    filter(fishmethod_other == 1) %>%
    select(strataCode, year, hhwt) %>%
    mutate(INDICATOR = 'FSHOTHR',
           UNIT_MEASURE = 'N'
    )
  
  hhld <- rbind(fisheries_mthother_nat, fisheries_mthother_str)
  
  hhld <- hhld %>%
    group_by(year, strataCode, INDICATOR, UNIT_MEASURE) %>%
    summarise(hhtotal = round(sum(hhwt), 0))
  
  return(hhld)
  
}



