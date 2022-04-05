# By Hansini Munasinghe - March 2022
# Code for getting, analyzing, and mapping Census data
# for project - Route 48

# Set up===============================================================

# install.packages("tidycensus")
# install.packages("tigris")
# install.packages("sf")
# install.packages("cartography")
# install.packages("dichromat")
# install.packages("mapview")
# install.packages("tmap")
# install.packages("openxlsx")

library(ggplot2)
library(tidycensus)
library(tidyverse)
library(tigris, options(tigris_use_cache = TRUE))
library(sf)
library(readxl)
library(mapview)
library(tmap)
library(openxlsx)
# library(dichromat)
# library(cartography)

# Census API key - only need to install once
# census_api_key("1b6b78f60660a226f7b5b0f899ba14e4b8a52e35", install = TRUE)

# tracts for analysis (along R48) ============================================
tracts_r48 <- c(
  "Census Tract 88, King County, Washington",
  "Census Tract 76, King County, Washington",
  "Census Tract 52.01, King County, Washington",
  "Census Tract 77, King County, Washington",
  "Census Tract 87, King County, Washington",
  "Census Tract 64, King County, Washington",
  "Census Tract 95, King County, Washington",
  "Census Tract 62, King County, Washington",
  "Census Tract 94, King County, Washington" ,
  "Census Tract 53.07, King County, Washington",
  "Census Tract 79.01, King County, Washington",
  "Census Tract 53.05, King County, Washington",
  "Census Tract 44.02, King County, Washington",
  "Census Tract 43.02, King County, Washington",
  "Census Tract 53.06, King County, Washington",
  "Census Tract 79.02, King County, Washington",
  "Census Tract 53.03, King County, Washington",
  "Census Tract 89, King County, Washington",
  "Census Tract 53.04, King County, Washington",
  "Census Tract 90, King County, Washington"
)	


# available vars  =============================================================
vars2 <- load_variables(2020, "acs5/subject")

# test code ===================================================================

king_tracts <- tracts(state = "WA", county = "King")
plot(king_tracts$geometry)
mapview(king_tracts)

# Race as % ===========================================================

race_vars <- c(
  white = "B02008_001",
  black = "B02009_001",
  aian  = "B02010_001",
  asian = "B02011_001",
  nhpi  = "B02012_001",
  other = "B02013_001"
)

king_race <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King",
  geometry = TRUE,
  variables = race_vars,
  summary_var = "B02001_001" # denominator 
)

# normalize columns
king_race_perc <- king_race %>%
  mutate(race_percent = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, race_percent)

king_race_asian <- king_race_perc %>%
  filter(variable == "asian")

plot(king_race_asian["race_percent"], main = "% Asian by Census Tract in King County")
mapview(king_race_asian)

ggplot(king_race_asian, aes(fill=race_percent))+
  geom_sf()

tm_shape(king_race_asian) + 
  tm_polygons()

tm_shape(king_race_perc, geometry) + 
  tm_polygons(col = "race_percent") +
  tm_facets(by = "variable", scale.factor = 6) + 
  tm_fill(col = "percent",
          style = "quantile",
          n = 6,
          palette = "Blues",
          title = "Percent (2015-2019 ACS)",) + 
  tm_layout(bg.color = "grey", 
            legend.position = c(-0.7, 0.2),
            panel.label.bg.color = "white")

# route race
r48_race <- king_race_perc %>%
  filter(NAME %in% tracts_r48)

r48_race %>%
  filter(variable == "asian") %>%
  ggplot(aes(fill=race_percent))+
    geom_sf()

r48_race_asian <- r48_race %>%
  filter(variable == "asian")

mapview(r48_race)
mapview(king_race)

# QC data =================================================================

king_race_qc <- spread(king_race_perc[,1:3, drop = TRUE], variable, race_percent)

# write.xlsx(king_race_qc, file = "I:\\Projects\\Active\\_Seattle\\_SDOT\\Transit Plus Multimodal Program Outreach\\route 48\\01_public involvement plan\\Demographics analysis\\data_tables_qc\\race.xlsx",
#            colNames = TRUE, rowNames = TRUE, append = FALSE, overwrite = TRUE)
           

# FINAL MAPS =========================

# Race (POC)
# income
# language
# disability status
# vehicle access

# 
# Characteristic	Table
# Age	DP05 – 2020 ACS 5-year estimate
# Sex	DP05 – 2020 ACS 5-year estimate
# Race and ethnicity	DP05 – 2020 ACS 5-year estimate
# Income	S1901, S1701 – 2020 ACS 5-year estimate
# Languages spoken at home	B16001 – 2015 ACS 5-year estimate
# Disability status	S1810 – 2020 ACS 5-year estimate
# Internet access	S2801 – 2020 ACS 5-year estimate
# Vehicle access	DP04 – 2020 ACS 5-year estimate
# Housing tenure	DP04 – 2020 ACS 5-year estimate


# Primary: RGB 22/145/208
# Secondary: RGB 225/217/30 
# Tertiary: RGB 128/130/132
# Four: RGB 0/168/93