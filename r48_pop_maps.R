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
# install.packages("naniar")
# install.packages("terra")
# install.packages("mapboxapi")
# install.packages("raster")

library(ggplot2)
library(tidycensus)
library(tidyverse)
library(tigris, options(tigris_use_cache = TRUE))
library(sf)
library(readxl)
library(mapview)
library(tmap)
library(openxlsx)
library(naniar)
library(terra)
library(mapboxapi)
library(raster)
# library(dichromat)
# library(cartography)

# Census API key - only need to install once
# census_api_key("1b6b78f60660a226f7b5b0f899ba14e4b8a52e35", install = TRUE)

# Mapbox API
 # mb_access_token("pk.eyJ1IjoiaGFuc2ltdW5hc2luZ2hlIiwiYSI6ImNsMXp2YTUybjBxa2MzaW8zaHMyZHk4bHMifQ.kRrPwRGdUZn77sWWYjptDA", 
 #                install = T, overwrite = T)

# filepaths etc.
savemaps = "I:/Projects/Active/_Seattle/_SDOT/Transit Plus Multimodal Program Outreach/route 48/01_public involvement plan/Demographics analysis/Routput_maps"

# available vars  =============================================================
vars <- load_variables(2020, "acs5")
vars_subject <- load_variables(2020, "acs5/subject")
vars_profile <- load_variables(2020, "acs5/profile")

# LIST OF MAPS ===========================================================

# Race (POC) 
# income - test map for poverty done
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
# Primary: RGB 22/145/208 #1691D0 (blue)
# Secondary: RGB 225/217/30 #E1D91E (yellow)
# Tertiary: RGB 128/130/132 #808284 (grey)
# Four: RGB 0/168/93 #00A85D (green)

# Single hue scale from blue 
#1691d0
#4ca2d9
#6eb2e3
#8cc3ec
#a8d5f5
#c4e6ff
client_blues = c("#1691d0", "#4ca2d9", "#6eb2e3", "#8cc3ec", "#a8d5f5", "#c4e6ff")



# tracts for analysis (along R48) NOT CONFIRMED ============================================
tracts_r48 <- c(
  "Census Tract 88, King County, Washington",
  "Census Tract 76, King County, Washington",
  "Census Tract 77, King County, Washington",
  "Census Tract 87, King County, Washington",
  "Census Tract 64, King County, Washington",
  "Census Tract 52.02, King County, Washington",
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

# testing tracts
king_pop <- 
  get_acs( # ACS 2020 5 year estimate 
    geography = "tract",
    state = "WA",
    county = "King",
    geometry = TRUE,
    year = 2020,
    variables =  c(pop = "B01001_001"), #estimate, not % #  mutate(_perc = 100 * (estimate / summary_est))
  )
  
r48_pop <- king_pop %>% filter(NAME %in% tracts_r48)

plot(king_pop$geometry)
mapview(king_pop) # use this view to identify tracts
mapview(r48_pop, zcol = "estimate")

# RACE =========================================
# side by side
# dot map 
# POC - not white and not hispanic/latinx (lower priority)

# Race vars - 
# B03002_001	Estimate!!Total:	
# B03002_003	Estimate!!Total:!!Not Hispanic or Latino:!!White alone	
# B03002_004	Estimate!!Total:!!Not Hispanic or Latino:!!Black or African American alone	
# B03002_005	Estimate!!Total:!!Not Hispanic or Latino:!!American Indian and Alaska Native alone	
# B03002_006	Estimate!!Total:!!Not Hispanic or Latino:!!Asian alone
# B03002_007	Estimate!!Total:!!Not Hispanic or Latino:!!Native Hawaiian and Other Pacific Islander alone
# B03002_008	Estimate!!Total:!!Not Hispanic or Latino:!!Some other race alone
# B03002_009	Estimate!!Total:!!Not Hispanic or Latino:!!Two or more races:	
# B03002_010	Estimate!!Total:!!Not Hispanic or Latino:!!Two or more races:!!Two races including Some other race	
# B03002_011	Estimate!!Total:!!Not Hispanic or Latino:!!Two or more races:!!Two races excluding Some other race, and three or more races	
# B03002_012	Estimate!!Total:!!Hispanic or Latino:

king_race <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King",
  variables = c(
    Latino = "B03002_012",
    white = "B03002_003",
    Black = "B02001_003",
    aian = "B03002_005",
    Asian = "B03002_006",
    nhpi = "B03002_007",
    other = "B03002_008", 
    multi = "B03002_009"
    ),
  summary_var = "B03002_001",
  year = 2020,
  geometry = TRUE
  ) %>%
  filter(NAME != "Census Tract 9901, King County, Washington") %>%
  mutate(percent = 100 * (estimate / summary_est))


# Viz - king county dot map
# king_dots <- king_race %>%
#   as_dot_density(
#     value = "estimate",
#     values_per_dot = 10,
#     group = "variable"
#   )
# 
# background_tracts <- filter(king_race, variable == "white")
# 
# tm_shape(background_tracts) + 
#   tm_polygons(col = "white", 
#               border.col = "grey") + 
#   tm_shape(king_dots) +
#   tm_dots(col = "variable", 
#           palette = "Set1",
#           size = 0.005, 
#           title = "1 dot = 100 people") + 
#   tm_layout(legend.outside = TRUE,
#             title = "Race & Ethnicity \nin King County, WA \n(data: 2020 US Census)")

# r48 dot map

r48_race <- king_race %>%   
  filter(NAME %in% tracts_r48) %>%
  filter(variable == "Asian" | variable == "white" | variable == "Black" | variable == "Latino")

r48_dots <- r48_race %>%
  as_dot_density(
    value = "estimate",
    values_per_dot = 1,
    group = "variable"
  )

r48_tracts <- filter(r48_race, variable == "white")

 
tm_shape(r48_dots) +
  tm_dots(col = "variable", 
          palette = "Set1",
          size = 0.00005, 
          title = "1 Dot = 1 Person",
          jitter = .2,) + 
  tm_layout(legend.outside = F,
            #title = "Race & Ethnicity \nalong Route 48 \n(data: 2016-2020 ACS)"
            ) +
  tm_shape(r48_tracts) + 
  tm_polygons(col = "white", alpha = 0, 
              border.col = "grey20") 

# If you don't have a Mapbox style to use, replace style_id with "light-v9"
# and username with "mapbox".  If you do, replace those arguments with your 
# style ID and user name.

r48_tiles_location <- r48_race %>% 
  filter(variable=="white") %>%
  select(geometry)

r48_tiles <- get_static_tiles(
  location = r48_race,
  zoom = 12,
  style_id = "light-v10",
  username = "mapbox",
  buffer_dist = 5000,
  style_url = NULL,
)

# outfile <- writeRaster(r48_tiles, filename='grid.tif', format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))



tm_shape(r48_tiles) +
  tm_rgb() + 
  tm_shape(r48_dots) +
  tm_dots(col = "variable", 
          palette = "Set1",
          size = 0.00005, 
          title = "1 Dot = 1 Person",
          jitter = .2,) + 
  tm_layout(legend.outside = T,
            #title = "Race & Ethnicity \nalong Route 48 \n(data: 2016-2020 ACS)"
  ) +
  tm_shape(r48_tracts) + 
  tm_polygons(col = "white", alpha = 0, 
              border.col = "grey20") 

king_race_select <- king_race %>%
  filter(variable == "asian" | variable == "white" | variable == "black")
  
ggplot(king_race_select, aes(fill= percent))+
    geom_sf() +
    theme_void() +
    labs(
      title = "Race % along Route 48",
      # subtitle = #"Percentage of families whose income in the last 12 months \nis below the poverty level
      #   "Data: 2016-2020 5-year ACS"
    ) +
    scale_fill_gradient(
      # colours = client_blues,
      low = "grey95", high = "#1691d0",
      na.value = "#808284",
  #     name =
  #       "Percentage of families whose
  # income in the last 12 months
  # is below the poverty level",
      guide = "colourbar") + 
    facet_wrap(vars(variable))


ggsave(filename = "race_messy_few.jpg", 
       plot = last_plot(),
       device = "jpg",
       path = savemaps)



# POVERTY ========================================================================================
# DP03_0119 - Estimate!!PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL!!All families	
# DP03_0119P	- Percent!!PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL!!All families	
# note - didn't use vars Emma identified - S1901, S1701 – 2020 ACS 5-year estimate

# change to 200% of poverty (using Emma's vars)
# S1701_C01_001	Estimate!!Total!!Population for whom poverty status is determined	
# S1701_C01_042	Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!200 percent of poverty level	

king_poverty <- 
  get_acs(
    geography = "tract",
    state = "WA",
    county = "King",
    geometry = TRUE,
    year = 2020,
    variables =  c(poverty200_est = "S1701_C01_042"), 
    summary_var = "S1701_C01_001",
    cb = TRUE,
  ) %>%
  mutate(poverty200_perc = 100 * (estimate / summary_est)) %>%
  # if not in r48 change to NA
  mutate(r48tract = if_else(NAME %in% tracts_r48, 1, -1)) %>%
  mutate(poverty200_perc_viz = r48tract * poverty200_perc) %>%
  replace_with_na_at(.vars = c("poverty200_perc_viz"),
                     condition = ~.x < 0)  
#viz
ggplot(king_poverty, aes(fill= poverty200_perc_viz))+
  geom_sf() +
  theme_void() +
  labs(
    title = "Income and Poverty along Route 48",
    subtitle = #"Percentage of families whose income in the last 12 months \nis below the poverty level
      "Data: 2016-2020 5-year ACS"
  ) +
  scale_fill_gradient(
    # colours = client_blues,
    low = "azure", high = "#1691d0",
    na.value = "#808284",
    name = 
      "Percentage of families whose 
income in the last 12 months
is below the poverty level",
    guide = "colourbar")
 
ggsave(filename = "poverty_othertractsingrey.jpg", 
       plot = last_plot(),
       device = "jpg",
       path = savemaps)


r48_poverty <- king_poverty %>%
  filter(NAME %in% tracts_r48) 

r48_poverty2 <- king_poverty %>% # show percent for r48 tract, empty for rest
  mutate(show =  case_when (
    NAME %in% tracts_r48 ~ "show",  
    TRUE ~ "no")) %>%
  mutate(perc_viz = ) 

# viz
ggplot(r48_poverty, aes(fill= estimate))+
  geom_sf() +
  theme_void() +
  labs(
    title = "Income and Poverty along Route 48",
    subtitle = #"Percentage of families whose income in the last 12 months \nis below the poverty level
    "Data: 2016-2020 5-year ACS"
    ) +
  scale_fill_gradient(
  # colours = client_blues,
    low = "azure", high = "#1691d0",
    na.value = "#808284",
    name = 
"Percentage of families whose 
income in the last 12 months
is below the poverty level",
    guide = "colourbar")

ggsave(filename = "poverty_azure2blue.jpg", 
       plot = last_plot(),
       device = "jpg",
       path = savemaps)

ggplot(r48_poverty, aes(fill= estimate))+
  geom_sf() +
  theme_void() +
  labs(title = "% of families below poverty level along Route 48") +
  scale_fill_gradient(
    # colours = client_blues,
    high = "#000213", low = "#1691d0",
    na.value = "#808284",
    name = "Percentage",
    guide = "colourbar")

ggsave(filename = "poverty_dark.jpg", 
       plot = last_plot(),
       device = "jpg",
       path = savemaps)


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
    year = 2020,
    variables =  c(limeng_est = "C16002_001"), #estimate, not % #  mutate(_perc = 100 * (estimate / summary_est))
  ) %>%
  filter(!row_number() %in% c(398)) %>% # weird empty row
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
    year = 2020,
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

ggsave(filename = "disability.jpg", 
       plot = last_plot(),
       device = "jpg",
       path = savemaps)

# VEHICLE USE ================================================================
# Vehicle access	DP04 – 2020 ACS 5-year estimate (from Emma)
# var -DP04_0058P	Percent!!VEHICLES AVAILABLE!!Occupied housing units!!No vehicles av

king_vehicles <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King",
  year = 2020,
  geometry = TRUE,
  variables = c(novehicle_perc = "DP04_0058P"),
  )

# # interactive mapview
# mapview(king_vehicles)

# select tracts on route 
r48_veh <- king_vehicles %>%
  filter(NAME %in% tracts_r48)

# viz
ggplot(r48_veh, aes(fill= estimate))+
  geom_sf() +
  theme_void() +
  labs(title = "% of households with no vehicles along Route 48") +
  scale_fill_gradientn(colours = c("azure3", "deepskyblue4")) 

ggsave(filename = "vehicle.jpg", 
       plot = last_plot(),
       device = "jpg",
       path = savemaps)

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
           
ggplot(r48_poverty, aes(fill= estimate))+
  geom_sf() +
  theme_void() +
  labs(
    title = "Income and Poverty along Route 48",
    subtitle = #"Percentage of families whose income in the last 12 months \nis below the poverty level
      "Data: 2016-2020 5-year ACS"
  ) +
  scale_fill_gradient(
    # colours = client_blues,
    low = "white", high = "red",
    na.value = "#808284",
    name = 
      "Percentage of families whose 
income in the last 12 months
is below the poverty level",
    guide = "colourbar")
