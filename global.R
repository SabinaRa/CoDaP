
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(stringr)
library(readr)
library(DT)
library(tidyverse)
library(haven)
library(plotly)
library(dplyr)
library(stringr)
library(readxl)
library(scales)
library(ggthemes)
library(leaflet)
library(reshape2)
library(leaflet.minicharts)
library(treemap)


options(digits.secs=2) 

data <- read_excel("./data/COVID-19-geographic-disbtribution-worldwide-2020-05-10.xlsx")

#data adjustment, preparation to implement join
data$countriesAndTerritories <- stringr::str_replace_all(data$countriesAndTerritories, "[_]", " ")
data <- data %>% mutate(geoId =replace(geoId, geoId=="UK", "GB")) 
data <- data %>% mutate(continentExp =replace(continentExp, continentExp=="America", "Americas")) 

countries <- read.csv("./data/countries.csv", header = TRUE, encoding= "UTF-8", stringsAsFactors = FALSE)
continents <- read.csv("./data/continents.csv", header = TRUE, encoding= "UTF-8", stringsAsFactors = FALSE)

#statadata adjustment, preparation to implement join
statadata <- read_dta("./data/GlobalBehaviorsPerceptions_Dataset_Apr22_2020.dta")
statadata <- statadata %>% mutate(CountryofLiving =replace(CountryofLiving, CountryofLiving=="United States", "United States of America")) %>% mutate(CountryofLiving =replace(CountryofLiving, CountryofLiving=="Czech Republic", "Czechia")) %>% mutate(CountryofLiving =replace(CountryofLiving, CountryofLiving=="Northern Macedonia", "North Macedonia")) %>% mutate(CountryofLiving =replace(CountryofLiving, CountryofLiving=="Republic of Moldova", "Moldova"))  


#preparation to implement join
bycountry_data <- data %>% distinct(countriesAndTerritories,geoId)
bycountry_statadata <- statadata %>% group_by(CountryofLiving,iso2c) %>%  tally()  %>% dplyr::filter(n >=20)

# join by country code
bycountry <- bycountry_data %>% inner_join(bycountry_statadata, by = c("geoId" = "iso2c"))  


bycontinent_data <- data %>% distinct(continentExp)
bycontinent_statadata <- statadata %>% group_by(continent) %>%  tally()  %>% dplyr::filter(n >=20)

bycontinent <- bycontinent_data %>% inner_join(bycontinent_statadata, by = c("continentExp" = "continent")) 



