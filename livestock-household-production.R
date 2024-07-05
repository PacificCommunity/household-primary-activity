# Load setup files
source("functions/setup.R")


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


