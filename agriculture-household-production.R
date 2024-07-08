# Load setup file
source("functions/setup.R")

#### *************************** Agriculture households processing *********************** ####

pActivity_agriculture_hh <- pActivity %>%
  filter(agric == 1) %>%
  select(countryCode, year, strataCode, hhwt) %>%
  mutate(INDICATOR = 'HHAGRI')

pActivity_agriculture_vege <- pActivity %>%
  filter(agric_vege == 1) %>%
  select(countryCode, year, strataCode, hhwt) %>%
  mutate(INDICATOR = 'ARGVEG')

pActivity_agriculture_tube <- pActivity %>%
  filter(agric_tuber == 1) %>%
  select(countryCode, year, strataCode, hhwt) %>%
  mutate(INDICATOR = 'ARGTUB')

pActivity_agriculture_fruit <- pActivity %>%
  filter(agric_fruit == 1) %>%
  select(countryCode, year, strataCode, hhwt) %>%
  mutate(INDICATOR = 'ARGFRT')


pActivity_agriculture_combine <- rbind(
  pActivity_agriculture_hh,
  pActivity_agriculture_vege,
  pActivity_agriculture_tube,
  pActivity_agriculture_fruit
)

pActivity_agriculture_combine <- pActivity_agriculture_combine %>%
  group_by(countryCode, year, strataCode, INDICATOR) %>%
  summarise(totHouseholds = round(sum(hhwt), 0))

pActivity_agriculture_combine <- as.data.table(pActivity_agriculture_combine)

pActivity_agriculture_combine_cube <- cube(pActivity_agriculture_combine, j = round(sum(totHouseholds), 0), by = c("countryCode", "year", "strataCode", "INDICATOR"), id = FALSE )

pActivity_agriculture_combine_cube <- pActivity_agriculture_combine_cube %>%
  filter(!is.na(countryCode))

pActivity_agriculture_combine_cube <- pActivity_agriculture_combine_cube %>%
  filter(!is.na(year)) %>%
  rename(totHouseholds = V1)

pActivity_agriculture_combine_cube <- pActivity_agriculture_combine_cube %>%
  mutate_all(~replace(., is.na(.), "_T")) %>%
  filter(strataCode != "N")

pActivity_agriculture_combine_cube_DT <- pActivity_agriculture_combine_cube %>%
  rename(GEO_PICT=countryCode, TIME_PERIOD = year, STRATA = strataCode, INDICATOR = INDICATOR, OBS_VALUE = totHouseholds) %>%
  mutate(FREQ = "A", UNIT_MEASURE = "N", UNIT_MULT = "", OBS_STATUS = "", DATA_SOURCE = "", OBS_COMMENT = "", CONF_STATUS = "")

#Re-organizing the fields following the proper order

agriculture <- pActivity_agriculture_combine_cube_DT %>%
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

#Output agriculture table to excel csv file

write.csv(agriculture, "output/agriculture.csv", row.names = FALSE)
