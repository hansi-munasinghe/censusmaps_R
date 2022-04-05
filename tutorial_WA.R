
# Learning TidyCensus using tutorial 
# Trying to get data for Washington

# go back to video #2 for margins of error

# Set up===============================================================

# install.packages("tidycensus")
# install.packages("tigris")
# install.packages("sf")
# install.packages("cartography")
install.packages("dichromat")

library(ggplot2)
library(tidycensus)
library(tidyverse)
library(tigris, options(tigris_use_cache = TRUE))
library(sf)
library(readxl)
library(dichromat)
# library(cartography)

# Census API key - only need to install once
# census_api_key("1b6b78f60660a226f7b5b0f899ba14e4b8a52e35", install = TRUE)

# available vars  =============================================================
vars <- load_variables(2019, "acs5")

# King County Income ============================================================

#Get data
king_income <- get_acs( geography = "tract",
                        variables = "B19013_001",
                        state = "WA",
                        county = "King",
                        geometry = TRUE, # get geo info for mapping
                        cb = FALSE) # cartographic boundary vs. TIGER/Line shapefiles

# Remove water
st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}

king_water <- area_water("WA", "King County", class = "sf") 
king_erase <- st_erase(king_income, king_water)

# Viz
ggplot(king_erase, aes(fill = estimate)) + 
  geom_sf() + 
  theme_void() + 
  scale_fill_viridis_c(labels = scales::dollar)


# King county - roads =============================================
king_roads <- roads(state = "WA", county = "King")
plot(king_roads$geometry)

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
  summary_var = "B02001_001" # denominator (here, county pop)
)

# normalize columns
king_race_perc <- king_race %>%
  mutate(race_percent = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, race_percent)

king_race_asian <- king_race_perc %>%
  filter(variable == "asian")
plot(king_race_asian["race_percent"], main = "% Asian by Census Tract in King County")

# Viz Maps ==============================================

wa_counties <- counties("Washington", cb = TRUE) #cb is Cartographic boundary for water etc.
plot(wa_counties$geometry)


wa_tracts <- tracts("WA", cb = TRUE)
plot(wa_tracts$geometry)

ggplot(wa_tracts) + 
  geom_sf() +
  theme_void()

# plot data
plot(king_income["estimate"])


# County Map for Kate ===============================================
#Get data
ziplines <- get_acs(    geography = "zcta",
                        state = "WA",
                        # county = "Pierce",
                        variables = "B19013_001",
                        geometry = TRUE) #,  get geo info for mapping
                        # cb = FALSE) # cartographic boundary vs. TIGER/Line shapefiles

# Remove water
# st_erase <- function(x, y) {
#   st_difference(x, st_union(y))
# }
# 
# wa_water <- area_water("WA", class = "sf") 
# wa_erase <- st_erase(ziplines, wa_water)

# bring in Kate's data
t2p_data <- read_xlsx(
  "C:\\Users\\hmunasinghe\\OneDrive - PRR, Inc\\Projects\\censusmaps_R\\T2P_forKate\\T2P zip code data.xlsx",
  col_names = TRUE)

# create dataset
ziplines <- ziplines [ -c(3:5) ]

t2p_data$`GEOID` <- as.character(t2p_data$`zip code`)

zip_t2pdata <- st_as_sf(merge(t2p_data, ziplines)) #, all = TRUE))

# Viz


ggplot(zip_t2pdata , aes(fill = count)) + 
  geom_sf() + 
  theme_void() +
  scale_fill_gradientn(colours = c("azure3", "deepskyblue4"))



  scale_color_paletteer_d(dutchmasters, milkmaid) 
# +
 # scale_colour_hue(direction=-1)

#  scale_fill_viridis_c(direction=-1)
#  # + 
  # scale_fill_brewer(palette = "Blues")
  #  #(labels = scales::count)