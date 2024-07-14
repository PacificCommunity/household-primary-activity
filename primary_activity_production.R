#Load the source file

source("setup.R")
source("households.R")

#### ********************** Generate the total number of households *************************************************** ####

household <- households(hhld)
livestock <- livestock_hhld(hhld)
fisheries <- fisheries_hhld(hhld)
agriculture <- agriculture_hhld(hhld)

#Calculate percentage of households involved in raising livestock
livestock_percent <- livestock %>%
  rename(lvkhhtotal = hhtotal) %>%
  mutate(INDICATOR = 'HHCNT')

livestock_percent <- merge(household, livestock_percent, by = c("year", "strataCode", "INDICATOR"), all = TRUE)
livestock_percent$percent <- round(livestock_percent$lvkhhtotal/livestock_percent$hhtotal * 100,0)

livestock_percent <- livestock_percent %>%
  select(year, strataCode, INDICATOR, percent) %>%
  rename(hhtotal = percent) %>%
  mutate(INDICATOR = 'HHLVKPER',
         UNIT_MEASURE = 'PERCENT'
         )
  
#Calculate percentage of households involved in raising livestock
fisheries_percent <- fisheries %>%
  rename(fshhhtotal = hhtotal) %>%
  mutate(INDICATOR = 'HHCNT')

fisheries_percent <- merge(household, fisheries_percent, by = c("year", "strataCode", "INDICATOR"), all = TRUE)
fisheries_percent$percent <- round(fisheries_percent$fshhhtotal/fisheries_percent$hhtotal * 100,0)

fisheries_percent <- fisheries_percent %>%
  select(year, strataCode, INDICATOR, percent) %>%
  rename(hhtotal = percent) %>%
  mutate(INDICATOR = 'HHFSHPER',
         UNIT_MEASURE = 'PERCENT'
         )

#Calculate percentage of households involved in raising livestock
agriculture_percent <- agriculture %>%
  rename(arghhtotal = hhtotal) %>%
  mutate(INDICATOR = 'HHCNT')
  
agriculture_percent <- merge(household, agriculture_percent, by = c("year", "strataCode", "INDICATOR"), all = TRUE)
agriculture_percent$percent <- round(agriculture_percent$arghhtotal/agriculture_percent$hhtotal * 100,0)

agriculture_percent <- agriculture_percent %>%
  select(year, strataCode, INDICATOR, percent) %>%
  rename(hhtotal = percent) %>%
  mutate(INDICATOR = 'HHARGPER',
         UNIT_MEASURE = 'PERCENT'
         )

#Combine the tables together
household_combine <- rbind(household, livestock, fisheries, agriculture, livestock_percent, fisheries_percent, agriculture_percent)

#Add the remaining required columns
household_combine_final <- household_combine %>%
  rename(GEO_PICT= strataCode, TIME_PERIOD = year, OBS_VALUE = hhtotal) %>%
  mutate(FREQ = "A", UNIT_MULT = "", OBS_STATUS = "", OBS_COMMENT = "", CONF_STATUS = "", DATA_SOURCE = "Household Income and Expenditure Surveys")

#Re-organize the columns in the proper order
household_combine_final <- household_combine_final %>%
  select(
    FREQ, TIME_PERIOD, GEO_PICT, INDICATOR, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, CONF_STATUS  
  )

#Outputting the final table to a csv file
write.csv(household_combine_final, "output/household_primary_final.csv", row.names = FALSE)
