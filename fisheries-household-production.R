# Load setup files
source("functions/setup.R")

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
