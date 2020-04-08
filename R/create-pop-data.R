library(readr)
library(dplyr)
library(stringr)
library(here)

# Data from NHGIS
# 2014-2018 ACS

# load county populations
countypop <- read_csv(here::here("data", "nhgis0048_csv", "nhgis0048_ds239_20185_2018_county.csv")) %>% 
  rename(totalpop = AJWME001, county = COUNTY) %>% 
  mutate(fips = as.numeric(STATEA), 
         county = str_remove(county, " County"),
         county = str_remove(county, " Parish"),
         county = str_remove(county, " city")) %>% 
  select(county, fips, COUNTYA, totalpop)

# merge counties into combined reporting units
nyc <- filter(countypop, fips == 36 & COUNTYA %in% c("005", "047", "061", "081", "085")) %>% 
  summarise(totalpop = sum(totalpop)) %>% 
  mutate(county = "New York City", fips = 36)

kc <- filter(countypop, fips == 29 & COUNTYA %in% c("037", "047", "095", "165")) %>% 
  summarise(totalpop = sum(totalpop)) %>% 
  mutate(county = "Kansas City Combo", fips = 29)

#jop <- filter(countypop, fips == 29 & COUNTYA %in% c("097", "145")) %>% 
#  summarise(totalpop = sum(totalpop)) %>% 
#  mutate(county = "Joplin Combo", fips = 29)

# drop combined counties and add new rows
out <- countypop %>% 
  filter((fips == 36 & COUNTYA %in% c("005", "047", "061", "081", "085") | (fips == 29 & COUNTYA %in% c("037", "047", "095", "165"))) == F) %>% 
  bind_rows(nyc, kc) %>% 
  select(-COUNTYA)

# fix county in New Mexico
out[str_detect(out$county, " Ana") & out$fips == 35, "county"] <- "Doña Ana"

write_csv(out, here::here("data", "county_populations_nyt.csv"))
