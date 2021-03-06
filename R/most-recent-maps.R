library(readr)
library(dplyr)
library(purrr)
library(sf)
library(ggplot2)
library(rcartocolor)
library(ghibli)
library(lubridate)
library(here)
library(data.table)
library(stringr)
library(broom)

#### data preparation ####

# load most update NYT file
nyt <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  mutate(
    county = str_remove(county, " city"),
    county = if_else(fips %in% c(29510, 24510, 51620, 51600, 51770, 51760),
                     paste0(county, " City"),
                     county)
  ) %>% 
  select(-fips)

# keep only most recent counts
today <- filter(nyt, date == max(date))

# load prepared boundary polygons
bnd <- st_read(here::here("data", "nyt_boundaries.shp"), stringsAsFactors = F) %>% 
  filter(state %in% c("Alaska", "Hawaii") == F)

# load prepared state boundaries
states <- st_read(here("data", "state_boundaries.shp"))


# add today's counts to boundary file, format and drop AK and HI
covid <- left_join(bnd, today) %>% 
  mutate(
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
  )

#### raw count maps ####


# do a simple map of confirmed cases
ggplot(covid) +
  geom_sf(aes(fill = case_grp), size = 0) +
  geom_sf(data = states, fill = NA, lwd = 0.2) +
  scale_fill_manual(values = c("white", carto_pal(5, "Mint"))) +
  labs(title = "Confirmed COVID-19 Cases", subtitle = unique(today$date), 
       fill = "", caption = "Data from The New York Times\nMap by @BenInquiring") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank()
  ) +
  ggsave(here::here("maps", paste0("total-cases_", unique(today$date), ".png")), height = 4, width = 6.44)



# do a simple map of confirmed deaths
ggplot(covid) +
  geom_sf(aes(fill = death_grp), size = 0) +
  geom_sf(data = states, fill = NA, lwd = 0.2) +
  scale_fill_manual(values = c("white", carto_pal(4, "Peach"))) +
  labs(title = "Confirmed COVID-19 Deaths", subtitle = unique(today$date), 
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
#### Per capita maps ####

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
        cases_pc < 5 ~ "Less than 5",
        cases_pc >= 5 & cases_pc < 20 ~ "5 to 20",
        cases_pc >= 20 & cases_pc < 75 ~ "20 to 75",
        TRUE ~ "75 and up"
      ), 
      levels = c("None", "Less than 5", "5 to 20", "20 to 75", "75 and up")
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
  

# do a simple map of confirmed cases per 100 people
ggplot(covid_pop) +
  geom_sf(aes(fill = case_pc_grp), size = 0) +
  geom_sf(data = states, fill = NA, lwd = 0.2) +
  scale_fill_manual(values = c("white", carto_pal(4, "Mint"))) +
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



# deaths per capita
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


#### temporal maps ####

# write a custom function that adds necessary variables to county-slice of data
add_trend_vars <- function(county){
  if(nrow(county) < 8) NA else {
    county %>% 
      # calculate daily new cases and deaths, and add numeric "time" variable
      mutate(
        new_cases = cases - c(0, embed(cases, dimension = 2)[,2]),
        new_deaths = deaths - c(0, embed(deaths, dimension = 2)[,2]),
        time = 0:7
      ) %>% 
      # drop additional reference day from data
      filter(date > min(date))
  }
}



# get last week of covid data, plus extra day for comparison
last_week <- nyt %>% 
  filter(date > max(date) - 8) %>% 
  # run calculations within each county
  group_by(state, county) %>% 
  group_split() %>% 
  map(add_trend_vars) %>% 
  .[is.na(.) == F] %>% 
  bind_rows() %>% 
  # replace any negative values (due to reporting changes I think?) with 0
  mutate(
    new_cases = if_else(new_cases < 0, 0, new_cases),
    new_deaths = if_else(new_deaths < 0, 0, new_deaths)
  )


# get slope for time regression on new cases
last_week %>% 
  group_by(state, county) %>% 
  group_split() %>% 
  map_dbl(
    as_mapper(
      ~ lm(new_cases ~ time, data = .x)$coefficients[2]
    )
  ) -> case_trend

# get slope for time regression on new deaths
last_week %>% 
  group_by(state, county) %>% 
  group_split() %>% 
  map_dbl(
    as_mapper(
      ~ lm(new_deaths ~ time, data = .x)$coefficients[2]
    )
  ) -> death_trend

# create spatial file to map trends
last_week %>% 
  select(state, county) %>% 
  unique() %>% 
  cbind(case_trend) %>% 
  cbind(death_trend) %>% 
  as_tibble() %>% 
  # join to county boundaries
  right_join(bnd) %>% 
  # create mapping categories
  mutate(
    case_trend_cat = case_when(
      case_trend <= -10 ~ "10+ fewer",
      case_trend > -10 & case_trend <= -1 ~ "1-10 fewer",
      case_trend > -1 & case_trend < 1 ~ "No change",
      case_trend >= 1 & case_trend < 10 ~ "1-10 more",
      case_trend >= 10 ~ "10+ more",
      TRUE ~ "No cases"
    ),
    death_trend_cat = case_when(
      death_trend <= -1 ~ "1+ fewer",
      death_trend > -1 & death_trend <= -0.5 ~ "0.5-1 fewer",
      death_trend > -0.5 & death_trend < 0.5 ~ "No change",
      death_trend >= 0.5 & death_trend < 1 ~ "0.5-1 more",
      death_trend >= 1 ~ "1+ more",
      TRUE ~ "No cases"
    ),
    case_trend_cat = factor(case_trend_cat, levels = c("No cases",
                                                       "10+ fewer",
                                                       "1-10 fewer",
                                                       "No change",
                                                       "1-10 more",
                                                       "10+ more")),
    death_trend_cat = factor(death_trend_cat, levels = c("No cases",
                                                         "1+ fewer",
                                                         "0.5-1 fewer",
                                                         "No change",
                                                         "0.5-1 more",
                                                         "1+ more"))
  ) -> week_trends


# map of case trend in past week by county
ggplot() +
  geom_sf(data = week_trends, aes(geometry = geometry, fill = case_trend_cat), size = 0) +
  geom_sf(data = states, fill = NA, lwd = 0.2) +
  scale_fill_manual(values = c("white", carto_pal(5, "Geyser"))) +
  labs(title = "Confimed COVID-19 Cases in Past Week", 
       subtitle = paste0(max(nyt$date) - 7, " to ", max(nyt$date)), 
       fill = "Daily change\nin cases", 
       caption = "Data from The New York Times\nMap by @BenInquiring") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank()
  ) +
  ggsave(here::here("maps", paste0("cases-trend_", unique(today$date), ".png")), height = 4, width = 6.44)



# map of death trend in past week by county
ggplot() +
  geom_sf(data = week_trends, aes(geometry = geometry, fill = death_trend_cat), size = 0) +
  geom_sf(data = states, fill = NA, lwd = 0.2) +
  scale_fill_manual(values = c("white", carto_pal(5, "Geyser"))) +
  labs(title = "Confimed COVID-19 Cases in Past Week", 
       subtitle = paste0(max(nyt$date) - 7, " to ", max(nyt$date)), 
       fill = "Daily change\nin death", 
       caption = "Data from The New York Times\nMap by @BenInquiring") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank()
  ) +
  ggsave(here::here("maps", paste0("deaths-trend_", unique(today$date), ".png")), height = 4, width = 6.44)
