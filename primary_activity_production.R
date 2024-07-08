#Load the source file

source("functions/setup.R")

#### ********************** Generate the total number of households *************************************************** ####

pActivity_HH <- pActivity %>%
  group_by(countryCode, year, strataCode) %>%
  summarise(households = round(sum(hhwt), 0))

pActivity_HH <- as.data.table(pActivity_HH)
pActivity_HH_cube <- cube(pActivity_HH, j = round(sum(households), 2), by = c("countryCode", "year", "strataCode"), id = FALSE )

pActivity_HH_cube <- pActivity_HH_cube %>%
  filter(!is.na(countryCode))

pActivity_HH_cube <- pActivity_HH_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)

pActivity_HH_cube <- pActivity_HH_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(strataCode != "N")

pActivity_HH_cube_DT <- pActivity_HH_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, STRATA = strataCode, OBS_VALUE = households) %>%
  mutate(FREQ = "A", INDICATOR = "HHCNT", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#### ****************************** Generate the number of households involved in livestock ************************* ####

#Generate Livestock households

pActivity_livestock <- pActivity %>%
  filter(livestock == 1) %>%
  group_by(countryCode, year, strataCode) %>%
  summarise(livestock_HH = round(sum(hhwt), 0))

pActivity_livestock <- as.data.table(pActivity_livestock)

pActivity_livestock_cube <- cube(pActivity_livestock, j = round(sum(livestock_HH), 2), by = c("countryCode", "year", "strataCode"), id = FALSE )

pActivity_livestock_cube <- pActivity_livestock_cube %>%
  filter(!is.na(countryCode))

pActivity_livestock_cube <- pActivity_livestock_cube %>%
  filter(!is.na(year)) %>%
  rename(livestock_hh = V1)

pActivity_livestock_cube <- pActivity_livestock_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(strataCode != "N")

pActivity_livestock_cube_DT <- pActivity_livestock_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, STRATA = strataCode, OBS_VALUE = livestock_hh) %>%
  mutate(FREQ = "A", INDICATOR = "HHLVK", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate Fisheries households

pActivity_fisheries <- pActivity %>%
  filter(fisheries == 1) %>%
  group_by(countryCode, year, strataCode) %>%
  summarise(fisheries_HH = round(sum(hhwt), 0))

pActivity_fisheries <- as.data.table(pActivity_fisheries)

pActivity_fisheries_cube <- cube(pActivity_fisheries, j = round(sum(fisheries_HH), 0), by = c("countryCode", "year", "strataCode"), id = FALSE )

pActivity_fisheries_cube <- pActivity_fisheries_cube %>%
  filter(!is.na(countryCode))

pActivity_fisheries_cube <- pActivity_fisheries_cube %>%
  filter(!is.na(year)) %>%
  rename(fisheries_hh = V1)

pActivity_fisheries_cube <- pActivity_fisheries_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(strataCode != "N")

pActivity_fisheries_cube_DT <- pActivity_fisheries_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, STRATA = strataCode, OBS_VALUE = fisheries_hh) %>%
  mutate(FREQ = "A", INDICATOR = "HHFSH", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate Agriculture households

pActivity_agriculture <- pActivity %>%
  filter(agric == 1) %>%
  group_by(countryCode, year, strataCode) %>%
  summarise(agriculture_HH = round(sum(hhwt), 0))

pActivity_agriculture <- as.data.table(pActivity_agriculture)

pActivity_agriculture_cube <- cube(pActivity_agriculture, j = round(sum(agriculture_HH), 0), by = c("countryCode", "year", "strataCode"), id = FALSE )

pActivity_agriculture_cube <- pActivity_agriculture_cube %>%
  filter(!is.na(countryCode))

pActivity_agriculture_cube <- pActivity_agriculture_cube %>%
  filter(!is.na(year)) %>%
  rename(agriculture_hh = V1)

pActivity_agriculture_cube <- pActivity_agriculture_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(strataCode != "N")

pActivity_agriculture_cube_DT <- pActivity_agriculture_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, STRATA = strataCode, OBS_VALUE = agriculture_hh) %>%
  mutate(FREQ = "A", INDICATOR = "HHARG", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Merging the households, livestock households, fisheries households and agriculture households 

primary_activity_households <- rbind(
  pActivity_HH_cube_DT,
  pActivity_livestock_cube_DT,
  pActivity_fisheries_cube_DT,
  pActivity_agriculture_cube_DT
)


#new_order <- c("FREQ", "TIME_PERIOD", "GEO_PICT", "INDICATOR", "STRATA", "OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "OBS_STATUS", "DATA_SOURCE", "OBS_COMMENT", "CONF_STATUS")
primary_activity_households_final <- primary_activity_households %>%
  select(FREQ, TIME_PERIOD, GEO_PICT, INDICATOR, STRATA, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, CONF_STATUS)

#Generate Percentage of households involved in livestock

livestock_HH <- merge(pActivity_HH_cube, pActivity_livestock_cube)
livestock_HH$households <- as.numeric(livestock_HH$households)
livestock_HH$livestock_hh <- as.numeric(livestock_HH$livestock_hh)


livestock_HH$percent <- round(livestock_HH$livestock_hh / livestock_HH$households * 100,2)

livestock_HH <- as.data.table(livestock_HH)    

livestock_HH <- livestock_HH %>%
  select(countryCode, year, strataCode, percent) %>%
  rename(OBS_VALUE = percent)

livestock_Percent_cube_DT <- livestock_HH %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, STRATA = strataCode, OBS_VALUE = OBS_VALUE) %>%
  mutate(FREQ = "A", INDICATOR = "HHLVPER", UNIT_MEASURE = "PERCENT", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate Percentage of households involved in fisheries

fisheries_HH <- merge(pActivity_HH_cube, pActivity_fisheries_cube)
fisheries_HH$households <- as.numeric(fisheries_HH$households)
fisheries_HH$fisheries_hh <- as.numeric(fisheries_HH$fisheries_hh)

fisheries_HH$percent <- round(fisheries_HH$fisheries_hh / fisheries_HH$households * 100,2)

fisheries_HH <- as.data.table(fisheries_HH)    

fisheries_HH <- fisheries_HH %>%
  select(countryCode, year, strataCode, percent) %>%
  rename(OBS_VALUE = percent)

fisheries_Percent_cube_DT <- fisheries_HH %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, STRATA = strataCode, OBS_VALUE = OBS_VALUE) %>%
  mutate(FREQ = "A", INDICATOR = "HHFSPER", UNIT_MEASURE = "PERCENT", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate Percentage of households involved in agriculture

agriculture_HH <- merge(pActivity_HH_cube, pActivity_agriculture_cube)
agriculture_HH$households <- as.numeric(agriculture_HH$households)
agriculture_HH$agriculture_hh <- as.numeric(agriculture_HH$agriculture_hh)

agriculture_HH$percent <- round(agriculture_HH$agriculture_hh / agriculture_HH$households * 100,2)
agriculture_HH <- as.data.table(agriculture_HH)    

agriculture_HH <- agriculture_HH %>%
  select(countryCode, year, strataCode, percent) %>%
  rename(OBS_VALUE = percent)

agriculture_Percent_cube_DT <- agriculture_HH %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, STRATA = strataCode, OBS_VALUE = OBS_VALUE) %>%
  mutate(FREQ = "A", INDICATOR = "HHAGPER", UNIT_MEASURE = "PERCENT", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Combine all Primary Activities tables 


pActivity_Combine_Final <- rbind(
  pActivity_HH_cube_DT,
  pActivity_livestock_cube_DT,
  livestock_Percent_cube_DT,
  pActivity_fisheries_cube_DT,
  fisheries_Percent_cube_DT,
  pActivity_agriculture_cube_DT,
  agriculture_Percent_cube_DT
)

pActivity_Combine_Final <- pActivity_Combine_Final %>%
  select(
    FREQ,
    TIME_PERIOD,
    GEO_PICT,
    INDICATOR,
    STRATA,
    OBS_VALUE,
    CONF_STATUS,
    OBS_COMMENT,
    UNIT_MEASURE,
    UNIT_MULT,
    OBS_STATUS,
    DATA_SOURCE
  )

write.csv(pActivity_Combine_Final, "../output/pActivity_Combine_Final.csv", row.names = FALSE)
