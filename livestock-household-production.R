# Load setup files
source("setup.R")
source("households.R")
source("livestock_details.R")

#### ****************************** Livestock processing ******************************* ####

household <- households(hhld)
livestock <- livestock_hhld(hhld)
livestock_pig <- livestock_pig(hhld)
livestock_chicken <- livestock_chicken(hhld)
livestock_duck <- livestock_duck(hhld)
livestock_other <- livestock_other(hhld)

# combine the dataframes to have one dataframe

livestock_combine <- rbind(
  household,
  livestock,
  livestock_pig,
  livestock_chicken,
  livestock_duck,
  livestock_other
)

livestock_combine_final <- livestock_combine %>%
  rename(GEO_PICT=strataCode, TIME_PERIOD = year, INDICATOR = INDICATOR, OBS_VALUE = hhtotal) %>%
  mutate(FREQ = "A", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "Household Income and Expenditure Surveys", OBS_COMMENT = "", CONF_STATUS = "")

#Re-organising the fields following the proper order

livestock_combine_final <- livestock_combine_final %>%
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

write.csv(livestock_combine_final, "output/livestock_combine_final.csv", row.names = FALSE)
