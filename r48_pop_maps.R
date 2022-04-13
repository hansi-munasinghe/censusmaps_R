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


# available vars  =============================================================
vars <- load_variables(2020, "acs5")
vars_subject <- load_variables(2020, "acs5/subject")
vars_profile <- load_variables(2020, "acs5/profile")

# LIST OF MAPS ===========================================================

# Race (POC)
# income
# language - test map done
# disability status - test map done
# vehicle access  - test map done


# Data	Tables
# Age	DP05 – 2020 ACS 5-year estimate
# Sex	DP05 – 2020 ACS 5-year estimate
# Race and ethnicity	DP05 – 2020 ACS 5-year estimate
# Income	S1901, S1701 – 2020 ACS 5-year estimate
# Languages spoken at home	B16001 – 2015 ACS 5-year estimate
# Disability status	S1810 – 2020 ACS 5-year estimate
# Internet access	S2801 – 2020 ACS 5-year estimate
# Vehicle access	DP04 – 2020 ACS 5-year estimate
# Housing tenure	DP04 – 2020 ACS 5-year estimate

# colors -
# Primary: RGB 22/145/208
# Secondary: RGB 225/217/30 
# Tertiary: RGB 128/130/132
# Four: RGB 0/168/93

# tracts for analysis (along R48) NOT CONFIRMED ============================================
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

# LANGUAGES ==============================================================
# var from Emma - # Languages spoken at home	B16001 – 2015 ACS 5-year estimate

# SPEAK ENGLISH LESS THAN VERY WELL
# var from tidycensus - 
# B16001_005 Estimate!!Total:!!Spanish:!!Speak English less than "very well"
# B99163_003	Estimate!!Total:!!Speak other languages:	ALLOCATION OF ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# not available - summary_var = "B99163_001"  #	ALLOCATION OF ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# C16002_001	Estimate!!Total:	HOUSEHOLD LANGUAGE BY HOUSEHOLD LIMITED ENGLISH SPEAKING STATUS


king_limeng <- 
  get_acs(
    geography = "tract",
    state = "WA",
    county = "King",
    geometry = TRUE,
    variables =  c(limeng_est = "C16002_001"), #estimate, not % #  mutate(_perc = 100 * (estimate / summary_est))
  ) %>%
  filter(!row_number() %in% c(398)) %>%
  filter(NAME %in% tracts_r48) 

tm_shape(king_limeng, 
         projection = sf::st_crs(26915)) + 
  tm_polygons() + 
  tm_bubbles(size = "estimate", alpha = 0.5, 
             #size.max=max(king_limeng$estimate)/2,
             scale = 1,
             col = "blue",
             breaks=c(-Inf, seq(0, 3000, by=500), Inf),
             title.size = "Limited English Households, ACS estimate") + 
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "bottom")


# tm_shape(king_limeng, 
#          projection = sf::st_crs(26915)) + 
#   tm_polygons() + 
#   tm_dots(size = "estimate", alpha = 0.5, 
#              #size.max=max(king_limeng$estimate)/2,
#              scale = 1,
#              col = "blue",
#              breaks=c(-Inf, seq(0, 3000, by=500), Inf),
#              title.size = "Limited English Households, ACS estimate") + 
#   tm_layout(legend.outside = TRUE,
#             legend.outside.position = "bottom")


# DISABILITY ===============================================================
# var from Emma - Disability status	S1810 – 2020 ACS 5-year estimate
# var from tidycensus - S1810_C02_001	Estimate!!With a disability!!Total civilian noninstitutionalized population	
# summary_var =  S1810_C01_001	Estimate!!Total!!Total civilian noninstitutionalized population

king_disability <- 
  get_acs(
    geography = "tract",
    state = "WA",
    county = "King",
    geometry = TRUE,
    variables = c(disability_est = "S1810_C02_001"),
    summary_var =  "S1810_C01_001" #	Estimate!!Total!!Total civilian noninstitutionalized population
  ) %>%
  mutate(disability_perc = 100 * (estimate / summary_est))

# select tracts on route 
r48_disability <- king_disability %>%
  filter(NAME %in% tracts_r48)

# viz
ggplot(r48_disability, aes(fill= disability_perc))+
  geom_sf() +
  theme_void() +
  labs(title = "% with a disability along Route 48") +
  scale_fill_gradientn(colours = c("azure3", "deepskyblue4")) 

# VEHICLE USE ================================================================
# Vehicle access	DP04 – 2020 ACS 5-year estimate (from Emma)
# var -DP04_0058P	Percent!!VEHICLES AVAILABLE!!Occupied housing units!!No vehicles av

king_vehicles <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King",
  geometry = TRUE,
  variables = c(novehicle_perc = "DP04_0058P"),
  )

# interactive mapview
mapview(king_vehicles)

# select tracts on route 
r48_veh <- king_vehicles %>%
  filter(NAME %in% tracts_r48)

# viz
ggplot(r48_veh, aes(fill= estimate))+
  geom_sf() +
  theme_void() +
  labs(title = "% of households with no vehicles along Route 48") +
  scale_fill_gradientn(colours = c("azure3", "deepskyblue4")) 


#STOP


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
           

