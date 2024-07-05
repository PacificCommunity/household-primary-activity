library(readxl)
library(openxlsx)
library(dplyr)
library(officer)
library(lubridate)
library(ggplot2)
library(haven)
library(data.table)
library(RSQLite)


#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

pActivity <- read_dta("data/SPC_REG-12_2012-2021_PRIMARY-ACTIVITIES.dta") %>%
             mutate(across(where(labelled::is.labelled), haven::as_factor))

mydb <- dbConnect(RSQLite::SQLite(), "data/data.db")

#### ************************* Creating subsidiary tables to be merged with main table *********************************** ####

rururb <- data.frame(
  rururbID = c(1, 2, 3),
  rururbCode = c("U", "R", "N"),
  rururb = c("Urban", "Rural", "National")
)

country <- data.frame(
  countryCode = c("CK", "FM", "FJ", "KI", "MH", "NR", "NU", "PW", "PG", "WS", "SB", "TK", "TO", "TV", "VU", "WF"), 
  country = c("Cook Islands", "Fed. States of Micronesia", "Fiji", "Kiribati", "Marshall Islands", "Nauru", "Niue", "Palau", "Papua New Guinea",  "Samoa", "Solomon Islands", "Tokelau", "Tonga", "Tuvalu", "Vanuatu", "Wallis & Futuna")
)

pActivity <- merge(pActivity, rururb, by = "rururb")
pActivity <- merge(pActivity, country, by = "country")

#### ********************** Generate the total number of households *************************************************** ####

pActivity_HH <- pActivity %>%
  group_by(countryCode, year, rururbCode) %>%
  summarise(households = round(sum(hhwt), 0))

pActivity_HH <- as.data.table(pActivity_HH)
pActivity_HH_cube <- cube(pActivity_HH, j = round(sum(households), 2), by = c("countryCode", "year", "rururbCode"), id = FALSE )

pActivity_HH_cube <- pActivity_HH_cube %>%
  filter(!is.na(countryCode))

pActivity_HH_cube <- pActivity_HH_cube %>%
  filter(!is.na(year)) %>%
  rename(households = V1)

pActivity_HH_cube <- pActivity_HH_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")
  
pActivity_HH_cube_DT <- pActivity_HH_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = households) %>%
  mutate(FREQ = "A", INDICATOR = "HHCNT", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")
    
#### ****************************** Generate the number of households involved in livestock ************************* ####

#Generate Livestock households

pActivity_livestock <- pActivity %>%
  filter(livestock == 1) %>%
  group_by(countryCode, year, rururbCode) %>%
  summarise(livestock_HH = round(sum(hhwt), 0))

pActivity_livestock <- as.data.table(pActivity_livestock)

pActivity_livestock_cube <- cube(pActivity_livestock, j = round(sum(livestock_HH), 2), by = c("countryCode", "year", "rururbCode"), id = FALSE )

pActivity_livestock_cube <- pActivity_livestock_cube %>%
  filter(!is.na(countryCode))

pActivity_livestock_cube <- pActivity_livestock_cube %>%
  filter(!is.na(year)) %>%
  rename(livestock_hh = V1)

pActivity_livestock_cube <- pActivity_livestock_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_livestock_cube_DT <- pActivity_livestock_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = livestock_hh) %>%
  mutate(FREQ = "A", INDICATOR = "HHLVK", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate Fisheries households

pActivity_fisheries <- pActivity %>%
  filter(fisheries == 1) %>%
  group_by(countryCode, year, rururbCode) %>%
  summarise(fisheries_HH = round(sum(hhwt), 0))

pActivity_fisheries <- as.data.table(pActivity_fisheries)

pActivity_fisheries_cube <- cube(pActivity_fisheries, j = round(sum(fisheries_HH), 0), by = c("countryCode", "year", "rururbCode"), id = FALSE )

pActivity_fisheries_cube <- pActivity_fisheries_cube %>%
  filter(!is.na(countryCode))

pActivity_fisheries_cube <- pActivity_fisheries_cube %>%
  filter(!is.na(year)) %>%
  rename(fisheries_hh = V1)

pActivity_fisheries_cube <- pActivity_fisheries_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_fisheries_cube_DT <- pActivity_fisheries_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = fisheries_hh) %>%
  mutate(FREQ = "A", INDICATOR = "HHFSH", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate Agriculture households

pActivity_agriculture <- pActivity %>%
  filter(agric == 1) %>%
  group_by(countryCode, year, rururbCode) %>%
  summarise(agriculture_HH = round(sum(hhwt), 0))

pActivity_agriculture <- as.data.table(pActivity_agriculture)

pActivity_agriculture_cube <- cube(pActivity_agriculture, j = round(sum(agriculture_HH), 0), by = c("countryCode", "year", "rururbCode"), id = FALSE )

pActivity_agriculture_cube <- pActivity_agriculture_cube %>%
  filter(!is.na(countryCode))

pActivity_agriculture_cube <- pActivity_agriculture_cube %>%
  filter(!is.na(year)) %>%
  rename(agriculture_hh = V1)

pActivity_agriculture_cube <- pActivity_agriculture_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_agriculture_cube_DT <- pActivity_agriculture_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = agriculture_hh) %>%
  mutate(FREQ = "A", INDICATOR = "HHARG", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Merging the households, livestock households, fisheries households and agriculture households 

primary_activity_households <- rbind(
  pActivity_HH_cube_DT,
  pActivity_livestock_cube_DT,
  pActivity_fisheries_cube_DT,
  pActivity_agriculture_cube_DT
)


#new_order <- c("FREQ", "TIME_PERIOD", "GEO_PICT", "INDICATOR", "URBANIZATION", "OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "OBS_STATUS", "DATA_SOURCE", "OBS_COMMENT", "CONF_STATUS")
primary_activity_households_final <- primary_activity_households %>%
  select(FREQ, TIME_PERIOD, GEO_PICT, INDICATOR, URBANIZATION, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, CONF_STATUS)
  
  #[, new_order]

#Generate Percentage of households involved in livestock

livestock_HH <- merge(pActivity_HH_cube, pActivity_livestock_cube)
livestock_HH$households <- as.numeric(livestock_HH$households)
livestock_HH$livestock_hh <- as.numeric(livestock_HH$livestock_hh)


livestock_HH$percent <- round(livestock_HH$livestock_hh / livestock_HH$households * 100,2)

livestock_HH <- as.data.table(livestock_HH)    

livestock_HH <- livestock_HH %>%
  select(countryCode, year, rururbCode, percent) %>%
  rename(OBS_VALUE = percent)

livestock_Percent_cube_DT <- livestock_HH %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = OBS_VALUE) %>%
  mutate(FREQ = "A", INDICATOR = "HHLVPER", UNIT_MEASURE = "PERCENT", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate Percentage of households involved in fisheries

fisheries_HH <- merge(pActivity_HH_cube, pActivity_fisheries_cube)
fisheries_HH$households <- as.numeric(fisheries_HH$households)
fisheries_HH$fisheries_hh <- as.numeric(fisheries_HH$fisheries_hh)


fisheries_HH$percent <- round(fisheries_HH$fisheries_hh / fisheries_HH$households * 100,2)

fisheries_HH <- as.data.table(fisheries_HH)    

fisheries_HH <- fisheries_HH %>%
  select(countryCode, year, rururbCode, percent) %>%
  rename(OBS_VALUE = percent)

fisheries_Percent_cube_DT <- fisheries_HH %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = OBS_VALUE) %>%
  mutate(FREQ = "A", INDICATOR = "HHFSPER", UNIT_MEASURE = "PERCENT", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Generate Percentage of households involved in agriculture

agriculture_HH <- merge(pActivity_HH_cube, pActivity_agriculture_cube)
agriculture_HH$households <- as.numeric(agriculture_HH$households)
agriculture_HH$agriculture_hh <- as.numeric(agriculture_HH$agriculture_hh)

agriculture_HH$percent <- round(agriculture_HH$agriculture_hh / agriculture_HH$households * 100,2)
agriculture_HH <- as.data.table(agriculture_HH)    

agriculture_HH <- agriculture_HH %>%
  select(countryCode, year, rururbCode, percent) %>%
  rename(OBS_VALUE = percent)

agriculture_Percent_cube_DT <- agriculture_HH %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = OBS_VALUE) %>%
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
    URBANIZATION,
    OBS_VALUE,
    CONF_STATUS,
    OBS_COMMENT,
    UNIT_MEASURE,
    UNIT_MULT,
    OBS_STATUS,
    DATA_SOURCE
  )

write.csv(pActivity_Combine_Final, "output/pActivity_Combine_Final.csv", row.names = FALSE)


#### ********************* Generate the number of households involved in Fishing *************************** ####

#Generating the number of households involved in fisheries

pActivity_fisheries <- pActivity %>%
  filter(fisheries == 1) %>%
  group_by(countryCode, year, rururbCode) %>%
  summarise(fisheries_HH = round(sum(hhwt), 0))

pActivity_fisheries <- as.data.table(pActivity_fisheries)

pActivity_fisheries_cube <- cube(pActivity_fisheries, j = round(sum(fisheries_HH), 2), by = c("countryCode", "year", "rururbCode"), id = FALSE )

pActivity_fisheries_cube <- pActivity_fisheries_cube %>%
  filter(!is.na(countryCode))

pActivity_fisheries_cube <- pActivity_fisheries_cube %>%
  filter(!is.na(year)) %>%
  rename(fisheries_hh = V1)

pActivity_fisheries_cube <- pActivity_fisheries_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_fisheries_cube_DT <- pActivity_fisheries_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = fisheries_hh) %>%
  mutate(FREQ = "A", INDICATOR = "HHFSH", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#### *************************** Fishing locations ********************************* ####

#Generating the number of households involved in fisheries inshore

pActivity_fisheries_locInshore <- pActivity %>%
  filter(fishloc_inshore == 1) %>%
  group_by(countryCode, year, rururbCode) %>%
  summarise(fisheries_inshore_HH = round(sum(hhwt), 0))

pActivity_fisheries_locInshore <- as.data.table(pActivity_fisheries_locInshore)

pActivity_fisheries_locInshore_cube <- cube(pActivity_fisheries_locInshore, j = round(sum(fisheries_inshore_HH), 2), by = c("countryCode", "year", "rururbCode"), id = FALSE )

pActivity_fisheries_locInshore_cube <- pActivity_fisheries_locInshore_cube %>%
  filter(!is.na(countryCode))

pActivity_fisheries_locInshore_cube <- pActivity_fisheries_locInshore_cube %>%
  filter(!is.na(year)) %>%
  rename(fisheries_inshore_hh = V1)

pActivity_fisheries_locInshore_cube <- pActivity_fisheries_locInshore_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_fisheries_locInshore_cube_DT <- pActivity_fisheries_locInshore_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = fisheries_inshore_hh) %>%
  mutate(FREQ = "A", INDICATOR = "FSHLOC", FISHING_ACTIVITY = "HHFINS", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")


#Generating the number of households involved in fisheries nearshore

pActivity_fisheries_locnearshore <- pActivity %>%
  filter(fishloc_nearshore == 1) %>%
  group_by(countryCode, year, rururbCode) %>%
  summarise(fisheries_nearshore_HH = round(sum(hhwt), 0))

pActivity_fisheries_locnearshore <- as.data.table(pActivity_fisheries_locnearshore)

pActivity_fisheries_locnearshore_cube <- cube(pActivity_fisheries_locnearshore, j = round(sum(fisheries_nearshore_HH), 2), by = c("countryCode", "year", "rururbCode"), id = FALSE )

pActivity_fisheries_locnearshore_cube <- pActivity_fisheries_locnearshore_cube %>%
  filter(!is.na(countryCode))

pActivity_fisheries_locnearshore_cube <- pActivity_fisheries_locnearshore_cube %>%
  filter(!is.na(year)) %>%
  rename(fisheries_nearshore_hh = V1)

pActivity_fisheries_locnearshore_cube <- pActivity_fisheries_locnearshore_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_fisheries_locnearshore_cube_DT <- pActivity_fisheries_locnearshore_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = fisheries_nearshore_hh) %>%
  mutate(FREQ = "A", INDICATOR = "FSHLOC",  FISHING_ACTIVITY = "HHFNRS", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")


#Generating the number of households involved in fisheries offshore

pActivity_fisheries_locoffshore <- pActivity %>%
  filter(fishloc_offshore == 1) %>%
  group_by(countryCode, year, rururbCode) %>%
  summarise(fisheries_offshore_HH = round(sum(hhwt), 0))

pActivity_fisheries_locoffshore <- as.data.table(pActivity_fisheries_locoffshore)

pActivity_fisheries_locoffshore_cube <- cube(pActivity_fisheries_locoffshore, j = round(sum(fisheries_offshore_HH), 2), by = c("countryCode", "year", "rururbCode"), id = FALSE )

pActivity_fisheries_locoffshore_cube <- pActivity_fisheries_locoffshore_cube %>%
  filter(!is.na(countryCode))

pActivity_fisheries_locoffshore_cube <- pActivity_fisheries_locoffshore_cube %>%
  filter(!is.na(year)) %>%
  rename(fisheries_offshore_hh = V1)

pActivity_fisheries_locoffshore_cube <- pActivity_fisheries_locoffshore_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_fisheries_locoffshore_cube_DT <- pActivity_fisheries_locoffshore_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = fisheries_offshore_hh) %>%
  mutate(FREQ = "A", INDICATOR = "FSHLOC", FISHING_ACTIVITY = "HHFOFS",  UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")


#Generating the number of households involved in fisheries other location

pActivity_fisheries_locother <- pActivity %>%
  filter(fishloc_other == 1) %>%
  group_by(countryCode, year, rururbCode) %>%
  summarise(fisheries_locother_HH = round(sum(hhwt), 0))

pActivity_fisheries_locother <- as.data.table(pActivity_fisheries_locother)

pActivity_fisheries_locother_cube <- cube(pActivity_fisheries_locother, j = round(sum(fisheries_locother_HH), 2), by = c("countryCode", "year", "rururbCode"), id = FALSE )

pActivity_fisheries_locother_cube <- pActivity_fisheries_locother_cube %>%
  filter(!is.na(countryCode))

pActivity_fisheries_locother_cube <- pActivity_fisheries_locother_cube %>%
  filter(!is.na(year)) %>%
  rename(fisheries_locother_hh = V1)

pActivity_fisheries_locother_cube <- pActivity_fisheries_locother_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_fisheries_locother_cube_DT <- pActivity_fisheries_locother_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, OBS_VALUE = fisheries_locother_hh) %>%
  mutate(FREQ = "A", INDICATOR = "FSHLOC", FISHING_ACTIVITY = "HHFOTS",  UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")


fishing_location_combine <- rbind(
  pActivity_fisheries_locInshore_cube_DT,
  pActivity_fisheries_locnearshore_cube_DT,
  pActivity_fisheries_locoffshore_cube_DT,
  pActivity_fisheries_locother_cube_DT
)

#### ***************************** Fishing method ************************************ ####

pActivity_fisheries_methodGleaning <- pActivity %>%
  filter(fishmethod_gleaning == 1) %>%
  select(countryCode, year, rururbCode, hhwt) %>%
  mutate(FISHING_ACTIVITY = 'FMGLEAN')

pActivity_fisheries_methodLine <- pActivity %>%
  filter(fishmethod_line == 1) %>%
  select(countryCode, year, rururbCode, hhwt) %>%
  mutate(FISHING_ACTIVITY = 'FMLINE')

pActivity_fisheries_methodNet <- pActivity %>%
  filter(fishmethod_net == 1) %>%
  select(countryCode, year, rururbCode, hhwt) %>%
  mutate(FISHING_ACTIVITY = 'FMNETS')

pActivity_fisheries_methodSpear <- pActivity %>%
  filter(fishmethod_spear == 1) %>%
  select(countryCode, year, rururbCode, hhwt) %>%
  mutate(FISHING_ACTIVITY = 'FMSPER')

pActivity_fisheries_methodOther <- pActivity %>%
  filter(fishmethod_other == 1) %>%
  select(countryCode, year, rururbCode, hhwt) %>%
  mutate(FISHING_ACTIVITY = 'FMOTHR')


fishing_method <- rbind(pActivity_fisheries_methodGleaning,
                        pActivity_fisheries_methodLine,
                        pActivity_fisheries_methodNet,
                        pActivity_fisheries_methodSpear,
                        pActivity_fisheries_methodOther
)

fishing_method <- fishing_method %>%
  group_by(countryCode, year, rururbCode, FISHING_ACTIVITY) %>%
  summarise(totHouseholds = round(sum(hhwt), 0))

fishing_method <- as.data.table(fishing_method)

fishing_method_cube <- cube(fishing_method, j = round(sum(totHouseholds), 0), by = c("countryCode", "year", "rururbCode", "FISHING_ACTIVITY"), id = FALSE )

fishing_method_cube <- fishing_method_cube %>%
  filter(!is.na(countryCode))

fishing_method_cube <- fishing_method_cube %>%
  filter(!is.na(year)) %>%
  rename(totHouseholds = V1)

fishing_method_cube <- fishing_method_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

fishing_method_cube_DT <- fishing_method_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, FISHING_ACTIVITY = FISHING_ACTIVITY, OBS_VALUE = totHouseholds) %>%
  mutate(FREQ = "A", INDICATOR = "FSHMET", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")


fishing <- rbind(fishing_location_combine, fishing_method_cube_DT)

fishing_combine_final <- fishing %>%
  select(
    FREQ,
    TIME_PERIOD,
    GEO_PICT,
    INDICATOR,
    FISHING_ACTIVITY,
    URBANIZATION,
    OBS_VALUE,
    CONF_STATUS,
    OBS_COMMENT,
    UNIT_MEASURE,
    UNIT_MULT,
    OBS_STATUS,
    DATA_SOURCE
  )

write.csv(fishing_combine_final, "output/fishing_combine_final.csv", row.names = FALSE)

#### ****************************** Livestock processing ******************************* ####

pActivity_livestock_hh <- pActivity %>%
  filter(livestock == 1) %>%
  select(countryCode, year, rururbCode, hhwt) %>%
  mutate(INDICATOR = 'HHLVSK')

pActivity_livestock_pig <- pActivity %>%
  filter(livestock_pig == 1) %>%
  select(countryCode, year, rururbCode, hhwt) %>%
  mutate(INDICATOR = 'LVKPIG')

pActivity_livestock_chicken <- pActivity %>%
  filter(livestock_chicken == 1) %>%
  select(countryCode, year, rururbCode, hhwt) %>%
  mutate(INDICATOR = 'LVKCHK')

pActivity_livestock_duck <- pActivity %>%
  filter(livestock_duck == 1) %>%
  select(countryCode, year, rururbCode, hhwt) %>%
  mutate(INDICATOR = 'LVKDCK')

pActivity_livestock_other <- pActivity %>%
  filter(livestock_other == 1) %>%
  select(countryCode, year, rururbCode, hhwt) %>%
  mutate(INDICATOR = 'LVKOTH')

# combine the dataframes to have one dataframe

pActivity_livestock_combine <- rbind(
  pActivity_livestock_hh,
  pActivity_livestock_pig,
  pActivity_livestock_chicken,
  pActivity_livestock_duck,
  pActivity_livestock_other
)

pActivity_livestock_combine <- pActivity_livestock_combine %>%
  group_by(countryCode, year, rururbCode, INDICATOR) %>%
  summarise(totHouseholds = round(sum(hhwt), 0))

pActivity_livestock_combine <- as.data.table(pActivity_livestock_combine)

pActivity_livestock_combine_cube <- cube(pActivity_livestock_combine, j = round(sum(totHouseholds), 0), by = c("countryCode", "year", "rururbCode", "INDICATOR"), id = FALSE )

pActivity_livestock_combine_cube <- pActivity_livestock_combine_cube %>%
  filter(!is.na(countryCode))

pActivity_livestock_combine_cube <- pActivity_livestock_combine_cube %>%
  filter(!is.na(year)) %>%
  rename(totHouseholds = V1)

pActivity_livestock_combine_cube <- pActivity_livestock_combine_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_livestock_combine_cube_DT <- pActivity_livestock_combine_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, INDICATOR = INDICATOR, URBANIZATION = rururbCode, OBS_VALUE = totHouseholds) %>%
  mutate(FREQ = "A", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Re-organising the fields following the proper order

livestock <- pActivity_livestock_combine_cube_DT %>%
  select(
    FREQ,
    TIME_PERIOD,
    GEO_PICT,
    INDICATOR,
    URBANIZATION,
    OBS_VALUE,
    CONF_STATUS,
    OBS_COMMENT,
    UNIT_MEASURE,
    UNIT_MULT,
    OBS_STATUS,
    DATA_SOURCE
  )

#Output livestock table to excel csv file

write.csv(livestock, "output/livestock.csv", row.names = FALSE)


#### *************************** Agriculture households processing *********************** ####

pActivity_agriculture_hh <- pActivity %>%
  filter(agric == 1) %>%
  select(countryCode, year, rururbCode, hhwt) %>%
  mutate(INDICATOR = 'HHAGRI')

pActivity_agriculture_vege <- pActivity %>%
  filter(agric_vege == 1) %>%
  select(countryCode, year, rururbCode, hhwt) %>%
  mutate(INDICATOR = 'ARGVEG')

pActivity_agriculture_tube <- pActivity %>%
  filter(agric_tuber == 1) %>%
  select(countryCode, year, rururbCode, hhwt) %>%
  mutate(INDICATOR = 'ARGTUB')

pActivity_agriculture_fruit <- pActivity %>%
  filter(agric_fruit == 1) %>%
  select(countryCode, year, rururbCode, hhwt) %>%
  mutate(INDICATOR = 'ARGFRT')


pActivity_agriculture_combine <- rbind(
  pActivity_agriculture_hh,
  pActivity_agriculture_vege,
  pActivity_agriculture_tube,
  pActivity_agriculture_fruit
)

pActivity_agriculture_combine <- pActivity_agriculture_combine %>%
  group_by(countryCode, year, rururbCode, INDICATOR) %>%
  summarise(totHouseholds = round(sum(hhwt), 0))

pActivity_agriculture_combine <- as.data.table(pActivity_agriculture_combine)

pActivity_agriculture_combine_cube <- cube(pActivity_agriculture_combine, j = round(sum(totHouseholds), 0), by = c("countryCode", "year", "rururbCode", "INDICATOR"), id = FALSE )

pActivity_agriculture_combine_cube <- pActivity_agriculture_combine_cube %>%
  filter(!is.na(countryCode))

pActivity_agriculture_combine_cube <- pActivity_agriculture_combine_cube %>%
  filter(!is.na(year)) %>%
  rename(totHouseholds = V1)

pActivity_agriculture_combine_cube <- pActivity_agriculture_combine_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(rururbCode != "N")

pActivity_agriculture_combine_cube_DT <- pActivity_agriculture_combine_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, URBANIZATION = rururbCode, INDICATOR = INDICATOR, OBS_VALUE = totHouseholds) %>%
  mutate(FREQ = "A", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Re-organizing the fields following the proper order

agriculture <- pActivity_agriculture_combine_cube_DT %>%
  select(
    FREQ,
    TIME_PERIOD,
    GEO_PICT,
    INDICATOR,
    URBANIZATION,
    OBS_VALUE,
    CONF_STATUS,
    OBS_COMMENT,
    UNIT_MEASURE,
    UNIT_MULT,
    OBS_STATUS,
    DATA_SOURCE
  )

#Output agriculture table to excel csv file

write.csv(agriculture, "output/agriculture.csv", row.names = FALSE)
