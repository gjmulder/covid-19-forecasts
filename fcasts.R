library(tidyverse)
library(lubridate)
library(stringi)

jhu_url_base <-
  paste0(
    "https://raw.githubusercontent.com/CSSEGISandData/",
    "COVID-19/master/csse_covid_19_data/",
    "csse_covid_19_time_series/"
  )

# cov19_confirmed_all <-
#   read_csv(paste0(jhu_url_base, "time_series_19-covid-Confirmed.csv"))

cov19_dead_all <-
  read_csv(paste0(jhu_url_base, "time_series_19-covid-Deaths.csv"))

# cov19_recovered_all <-
#   read_csv(paste0(jhu_url_base, "time_series_19-covid-Recovered.csv"))

cov19_dead_age <-
  read_csv("age_death_rate.csv")

world_pop <-
  read_csv("WPP2019_PopulationByAgeSex_Medium.csv") %>%
  filter(Time == 2018) %>%
  select(Location, AgeGrp, PopMale, PopFemale)

###########################################################################################################################

cov19_dead_all %>%
  select(-`Province/State`, -Lat, -Long) %>%
  rename(country = `Country/Region`) %>%
  group_by(country) %>%
  summarise_all(sum) ->
  cov19_dead

print("Unmatched countries (before renaming)")
print(setdiff(
  cov19_dead$country,
  intersect(cov19_dead$country, world_pop$Location)
))
patt <- c(
  "Andorra",
  "Bolivia",
  "Brunei",
  "Congo (Kinshasa)",
  "Cote d'Ivoire",
  "Cruise Ship",
  "Holy See",
  "Iran",
  "Korea, South",
  "Liechtenstein",
  "Moldova",
  "Monaco",
  "Reunion",
  "Russia",
  "San Marino",
  "Taiwan*",
  "US",
  "Vietnam"
)
repl <- c(
  NA,
  "Bolivia (Plurinational State of)",
  "Brunei Darussalam",
  "Congo",
  "Côte d'Ivoire",
  NA,
  NA,
  "Iran (Islamic Republic of)",
  "Republic of Korea",
  NA,
  "Republic of Moldova",
  NA,
  NA,
  "Russian Federation",
  NA,
  "China, Taiwan Province of China",
  "United States of America",
  "Viet Nam"
)
cov19_dead$Location <- stri_replace_all_fixed(cov19_dead$country, patt, repl, vectorize_all = FALSE)
print("Unmatched countries (after renaming)")
print(setdiff(
  cov19_dead$country,
  intersect(cov19_dead$country, world_pop$Location)
))

world_death_rate <-
  left_join(world_pop, cov19_dead_age)

world_death_cov19 <-
  left_join(world_death_rate, cov19_dead) %>%
  na.omit()
