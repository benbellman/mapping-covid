library(sf)
library(tigris)
library(here)
library(dplyr)

# load county boundaries
county_bnd <- counties(cb = T) %>% 
  st_as_sf()

# paste " City" at the end of six independent cities to distinguish them from counties
county_bnd[county_bnd$STATEFP == 24 & county_bnd$COUNTYFP == 510, "NAME"] <- "Baltimore City"
county_bnd[county_bnd$STATEFP == 29 & county_bnd$COUNTYFP == 510, "NAME"] <- "St. Louis City"
county_bnd[county_bnd$STATEFP == 51 & county_bnd$COUNTYFP == 600, "NAME"] <- "Fairfax City"
county_bnd[county_bnd$STATEFP == 51 & county_bnd$COUNTYFP == 620, "NAME"] <- "Franklin City"
county_bnd[county_bnd$STATEFP == 51 & county_bnd$COUNTYFP == 760, "NAME"] <- "Richmond City"
county_bnd[county_bnd$STATEFP == 51 & county_bnd$COUNTYFP == 770, "NAME"] <- "Roanoke City"

county_bnd <- select(county_bnd, STATEFP, NAME)

# combine NYC, re-add to counties
nyc <- filter(county_bnd, STATEFP == 36 & NAME %in% c("New York", "Kings", "Queens", "Bronx", "Richmond"))
county_bnd <- filter(county_bnd, !(STATEFP == 36 & NAME %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")))
nyc_cb <- tibble(STATEFP = "36", NAME = "New York City")
st_geometry(nyc_cb) <- st_union(nyc)
county_bnd <- rbind(county_bnd, nyc_cb)

# add KC as separate entity
kc <- tigris::places(29, cb = T) %>% 
  st_as_sf() %>% 
  filter(NAME %in% c("Kansas City")) %>% 
  select(STATEFP, NAME)
county_bnd <- st_difference(county_bnd, kc) %>% 
  select(STATEFP, NAME) %>% 
  unique()
county_bnd <- rbind(county_bnd, kc)

# load master FIPS file
fips <- read_csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_fips_master.csv")

# merge state_name to boundaries
county_bnd <- county_bnd %>%
  mutate(fips = as.numeric(STATEFP)) %>% 
  select(fips, NAME) %>% 
  filter(fips <= 56) %>% 
  left_join(select(fips, fips, state_name)) %>% 
  rename(county = NAME, state = state_name) %>% 
  mutate(state = ifelse(is.na(state), "District of Columbia", state)) %>% 
  st_transform(2163) %>% 
  st_simplify(dTolerance = 1000) #1km


# save boundaries
st_write(county_bnd, "~/Documents/Computer Backup/Repos/mapping-covid/data/nyt_boundaries.shp", delete_layer = T)


# get states and save as file to remove download step
states <- states(cb = T) %>% 
  st_as_sf() %>% 
  st_transform(2163) %>% 
  st_simplify(dTolerance = 2000) %>% #2km
  mutate(STATEFP = as.numeric(STATEFP)) %>% 
  filter(STATEFP <= 56) %>% 
  filter(STATEFP %in% c(2, 15) == F)

st_write(states, "~/Documents/Computer Backup/Repos/mapping-covid/data/state_boundaries.shp", delete_layer = T)




### Twitter tips
# From Alexis, better county polygons
#library(usmap)
#us_counties<-map_data("county")
#map<-county_map
#[Your data]$id<-as.character([yourdata]$fips)
#county_full <- left_join(county_map, [your data], by = "id")