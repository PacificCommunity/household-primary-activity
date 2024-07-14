# Load libraries

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

#Read in datafile
pActivity <- read_dta("data/SPC_REG-12_2012-2021_PRIMARY-ACTIVITIES_new.dta") %>%
  mutate(across(where(labelled::is.labelled), haven::as_factor))

subNational <- read.csv("data/subNational.csv")


#### ************************* Creating subsidiary tables to be merged with main table *********************************** ####

#rururb <- data.frame(
#  rururbID = c(1, 2, 3),
#  rururbCode = c("U", "R", "N"),
#  rururb = c("Urban", "Rural", "National")
#)

country <- data.frame(
  countryCode = c("CK", "FM", "FJ", "KI", "MH", "NR", "NU", "PW", "PG", "WS", "SB", "TK", "TO", "TV", "VU", "WF"), 
  country = c("Cook Islands", "Fed. States of Micronesia", "Fiji", "Kiribati", "Marshall Islands", "Nauru", "Niue", "Palau", "Papua New Guinea",  "Samoa", "Solomon Islands", "Tokelau", "Tonga", "Tuvalu", "Vanuatu", "Wallis & Futuna")
)

#Merging main table with the subsidiary tables

#pActivity <- merge(pActivity, rururb, by = "rururb")
pActivity <- merge(pActivity, country, by = "country")

pActivity$strataCode <- paste0(pActivity$countryCode,"-",pActivity$strata)
pActivity <- merge(pActivity, subNational, by = c("countryCode", "strataCode"))
