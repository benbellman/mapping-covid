library(readr)
library(dplyr)
library(purrr)
library(sf)
library(ggplot2)
library(gganimate)
#library(transformr)
library(rcartocolor)
library(ghibli)
library(tigris)
library(lubridate)
library(here)
library(data.table)
library(stringr)
library(tidyr)


# load prepared boundary polygons
bnd <- st_read(here::here("data", "nyt_boundaries.shp"), stringsAsFactors = F) %>% 
  # paste state and county together
  mutate(
    #unique_name = paste(county, state, sep = ", ")
  ) %>% 
  filter(state %in% c("Alaska", "Hawaii") == F)

# function that joins input table to "bnd" and returns spatial object
merge_bnd <- function(df) left_join(bnd, df) #%>% filter(is.na(date) == F)

# load most update NYT file
nyt <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  select(-fips) %>% 
  mutate(
    county = str_remove(county, " city")
    #unique_name = paste(county, state, sep = ", ")
  ) %>% 
  # split NYT data by date
  group_by(date) %>% 
  group_split(keep = T)

# join each date to boundaries and combine
covid <- map(nyt, merge_bnd) %>% 
  map(mutate, 
      date = max(date, na.rm = T),
      cases = ifelse(is.na(cases), 0, cases),
      deaths = ifelse(is.na(deaths), 0, deaths),
      case_grp = factor(
        case_when(
          cases < 1 ~ "None",
          cases >= 1 & cases < 10 ~ "1 to 9",
          cases >= 10 & cases < 100 ~ "10 to 99",
          cases >= 100 & cases < 1000 ~ "100 to 999",
          cases >= 100 & cases < 10000 ~ "1,000 to 9,999",
          TRUE ~ "10,000 and up"
        ),
        levels = c("None", "1 to 9", "10 to 99", "100 to 999", "1,000 to 9,999", "10,000 and up")
      ),
      death_grp = factor(
        case_when(
          deaths < 1 ~ "None",
          deaths >= 1 & deaths < 5 ~ "1 to 5",
          deaths >= 5 & deaths < 25 ~ "6 to 25",
          deaths >= 25 & deaths < 125 ~ "26 to 125",
          TRUE ~ "126 and up"),
        levels = c("None", "1 to 5", "6 to 25", "26 to 125", "126 and up")
      )
    ) %>% 
  do.call(rbind, args = .) %>% 
  arrange(date, state, county)

# get state boundaries
states <- states(cb = T) %>% 
  st_as_sf() %>% 
  st_transform(2163) %>% 
  st_simplify(dTolerance = 2000) %>% #2km
  mutate(STATEFP = as.numeric(STATEFP)) %>% 
  filter(STATEFP <= 56) %>% 
  filter(STATEFP %in% c(2, 15) == F)


covid <- filter(covid, date > as.Date("2020-03-31"))

# do animaiton of confirmed cases
ggplot(covid) +
  geom_sf(aes(fill = case_grp), 
          size = 0) +
  geom_sf(data = states, 
          fill = NA, lwd = 0.2) +
  scale_fill_manual(values = c("white", carto_pal(5, "Mint"))) +
  labs(title = "Confirmed COVID-19 Cases", subtitle = "{frame_time}",
       fill = "", caption = "Data from The New York Times\nMap by @BenInquiring") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank()
  ) +
  transition_time(date) -> cases_map


 <- animate(cases_map, fps = 4)
anim_save(here::here("animations", "total-cases.gif"), cases_map_ani)


# do a simple map of confirmed deaths
ggplot(covid) +
  geom_sf(aes(fill = death_grp), size = 0) +
  geom_sf(data = states, fill = NA, lwd = 0.2) +
  scale_fill_manual(values = c("white", carto_pal(4, "Peach"))) +
  labs(title = "Confirmed COVID-19 Deaths", 
       fill = "", caption = "Data from The New York Times\nMap by @BenInquiring") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank()
  ) +
  ggsave(here::here("maps", paste0("total-deaths_", unique(today$date), ".png")), height = 4, width = 6.44)



########## 
# Per capita maps

# load population data
pop <- read_csv(here::here("data", "county_populations_nyt.csv"))

# slice off and merge Kansas City and Joplin geography combinations
kc_source <- filter(covid, fips == 29 & county %in% c("Platte", "Cass", "Clay", "Jackson", "Kansas City")) 

kc <- kc_source %>% 
  st_drop_geometry() %>% 
  summarise(cases = sum(cases), deaths = sum(deaths), date = max(date)) %>% 
  mutate(county = "Kansas City Combo", fips = 29, state = "Missouri",
         case_grp = "100 to 999", death_grp = "1 to 5")

st_geometry(kc) <- st_union(kc_source)

# begin new boundaries for population data
covid_pop <- covid %>% 
  # drop kc rows from covid data and re-add combo
  filter((fips == 29 & county %in% c("Platte", "Cass", "Clay", "Jackson", "Kansas City")) == F) %>% 
  rbind(kc) %>% 
  # join with population totals
  left_join(pop) %>% 
  # add new columns
  mutate(
    cases_pc = cases / totalpop * 10000,
    deaths_pc = deaths / totalpop * 10000,
    case_pc_grp = factor(
      case_when(
        cases == 0 ~ "None",
        cases_pc < 2 ~ "Less than 2",
        cases_pc >= 2 & cases_pc < 5 ~ "2 to 5",
        cases_pc >= 5 & cases_pc < 15 ~ "5 to 15",
        TRUE ~ "15 and up"
      ), 
      levels = c("None", "Less than 2", "2 to 5", "5 to 15", "15 and up")
    ),
    death_pc_grp = factor(
      case_when(
        deaths == 0 ~ "None",
        deaths_pc < 0.5 ~ "Less than 0.5",
        deaths_pc >= 0.5 & deaths_pc < 1 ~ "0.5 to 1",
        deaths_pc >= 1 & deaths_pc < 5 ~ "1 to 5",
        TRUE ~ "5 and up"
      ), 
      levels = c("None", "Less than 0.5", "0.5 to 1", "1 to 5", "5 and up")
    )
  )


# do a simple map of confirmed cases per 10k people
ggplot(covid_pop) +
  geom_sf(aes(fill = case_pc_grp), size = 0) +
  geom_sf(data = states, fill = NA, lwd = 0.2) +
  scale_fill_manual(values = c("white", carto_pal(5, "Mint"))) +
  labs(title = "Confirmed COVID-19 Cases Per Capita", subtitle = unique(today$date), 
       fill = "Cases per\n10k people", 
       caption = "Data from The New York Times, American Community Survey\nMap by @BenInquiring") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank()
  ) +
  ggsave(here::here("maps", paste0("cases-per-capita_", unique(today$date), ".png")), height = 4, width = 6.44)



# deaths per 100k people
ggplot(covid_pop) +
  geom_sf(aes(fill = death_pc_grp), size = 0) +
  geom_sf(data = states, fill = NA, lwd = 0.2) +
  scale_fill_manual(values = c("white", carto_pal(4, "Peach"))) +
  labs(title = "Confirmed COVID-19 Deaths Per Capita", subtitle = unique(today$date), 
       fill = "Deaths per\n100k people", 
       caption = "Data from The New York Times, American Community Survey\nMap by @BenInquiring") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank()
  ) +
  ggsave(here::here("maps", paste0("deaths-per-capita_", unique(today$date), ".png")), height = 4, width = 6.44)
