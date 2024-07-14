# Load setup files
source("setup.R")
source("households.R")
source("fisheries_details.R")

#### ****************************** Livestock processing ******************************* ####

household <- households(hhld)
fisheries <- fisheries_hhld(hhld)



fisheries_inshore <- fisheries_inshore(hhld)
fisheries_nearshore <- fisheries_nearshore(hhld)
fisheries_offshore <- fisheries_offshore(hhld)
fisheries_otherloc <- fisheries_otherloc(hhld)

fisheries_gleaning <- fisheries_gleaning(hhld)
fisheries_line <- fisheries_line(hhld)
fisheries_net <- fisheries_net(hhld)
fisheries_spear <- fisheries_spear(hhld)
fisheries_mthOther <- fisheries_mthOther(hhld)

# combine the dataframes to have one dataframe

fisheries_combine <- rbind(
  household,
  fisheries,
  fisheries_inshore,
  fisheries_nearshore,
  fisheries_offshore,
  fisheries_otherloc,
  
  fisheries_gleaning,
  fisheries_line,
  fisheries_net,
  fisheries_spear,
  fisheries_mthOther
  
)


fisheries_combine_final <- fisheries_combine %>%
  rename(GEO_PICT=strataCode, TIME_PERIOD = year, INDICATOR = INDICATOR, OBS_VALUE = hhtotal) %>%
  mutate(FREQ = "A", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "Household Income and Expenditure Surveys", OBS_COMMENT = "", CONF_STATUS = "")

#Re-organising the fields following the proper order

fisheries_combine_final <- fisheries_combine_final %>%
  select(
    FREQ,
    TIME_PERIOD,
    GEO_PICT,
    INDICATOR,
    OBS_VALUE,
    CONF_STATUS,
    OBS_COMMENT,
    UNIT_MEASURE,
    UNIT_MULT,
    OBS_STATUS,
    DATA_SOURCE
  )

#Output livestock table to excel csv file

write.csv(fisheries_combine_final, "output/fisheries_combine_final.csv", row.names = FALSE)
