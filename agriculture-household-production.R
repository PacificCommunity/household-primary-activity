# Load setup files
source("setup.R")
source("households.R")
source("agriculture_details.R")

#### ****************************** Livestock processing ******************************* ####

household <- households(hhld)
agriculture <- agriculture_hhld(hhld)
agriculture_vege <- agriculture_vege(hhld)
agriculture_tuber <- agriculture_tuber(hhld)
agriculture_frut <- agriculture_frut(hhld)

# combine the dataframes to have one dataframe

agriculture_combine <- rbind(
  household,
  agriculture,
  agriculture_vege,
  agriculture_tuber,
  agriculture_frut
)


agriculture_combine_final <- agriculture_combine %>%
  rename(GEO_PICT=strataCode, TIME_PERIOD = year, INDICATOR = INDICATOR, OBS_VALUE = hhtotal) %>%
  mutate(FREQ = "A", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "Household Income and Expenditure Surveys", OBS_COMMENT = "", CONF_STATUS = "")

#Re-organising the fields following the proper order

agriculture_combine_final <- agriculture_combine_final %>%
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

write.csv(agriculture_combine_final, "output/agriculture_combine_final.csv", row.names = FALSE)
