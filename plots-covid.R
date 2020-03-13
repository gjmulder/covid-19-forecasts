library(tidyverse)
library(lubridate)
library(stringi)
library(directlabels)
library()
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

# https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationByAgeSex_Medium.csv
world_pop <-
  read_csv("WPP2019_PopulationByAgeSex_Medium.csv") %>%
  filter(Time == 2018) %>%
  select(Location, AgeGrp, PopMale, PopFemale)

hosp_beds <-
  read_csv("API_SH.MED.BEDS.ZS_DS2_en_csv_v2_821439.csv", skip=4)

###########################################################################################################################
#
# Rename countries for Covid data

cov19_dead_all %>%
  select(-`Province/State`, -Lat, -Long) %>%
  rename(country = `Country/Region`) %>%
  group_by(country) %>%
  summarise_all(sum) ->
  cov19_dead

print("Age - Unmatched countries (before renaming)")
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
print("Age - Unmatched countries (after renaming)")
print(setdiff(
  cov19_dead$Location,
  intersect(cov19_dead$Location, world_pop$Location)
))

# world_death_rate <-
#   left_join(world_pop, cov19_dead_age)
#
# world_death_cov19 <-
#   left_join(world_death_rate, cov19_dead) %>%
#   na.omit()

###########################################################################################################################
#
# Ratio deaths per old age

old_world_pop <-
  world_pop %>%
  filter(AgeGrp %in% c("60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+")) %>%
  group_by(Location) %>%
  summarise(population.1M = as.character(sum(PopMale + PopFemale)/1000))

pop_death <-
  inner_join(old_world_pop, cov19_dead) %>%
  mutate_if(is.double, function(d) return(d/as.integer(.$population.1M))) %>%
  select(-Location, -population.1M) %>%
  gather(date.str, deaths, -country) %>%
  filter(deaths > 0.001) %>%
  filter(! is.infinite(deaths)) %>%
  mutate(dt = mdy(date.str)) %>%
  group_by(country) %>%
  arrange(dt) %>%
  mutate(day.number = dplyr::row_number())

gg <-
  ggplot(pop_death, aes(x = day.number, y = deaths, colour = country)) +
  geom_line() +
  scale_y_log10() +
  xlab("Day number") +
  ylab("Deaths per 1M population greater than 59 years of age") +
  geom_dl(aes(label = country), method = list(dl.combine("last.points"), rot=-30, cex=0.7))

print(gg)

###########################################################################################################################
#
# Ratio deaths per hospital beds

print("Beds - Unmatched countries (before renaming)")
print(setdiff(
  cov19_dead$country,
  intersect(cov19_dead$country, hosp_beds$`Country Name`)
))
patt <- c(
  "Brunei",
  "Congo (Kinshasa)",
  "Cruise Ship",
  "Czechia",
  "Egypt",
  "French Guiana",
  "Holy See",
  "Iran",
  "Korea, South",
  "Martinique",
  "Reunion",
  "Russia",
  "Slovakia",
  "Taiwan*",
  "US"
)
repl <- c(
  "Brunei Darussalam",
  "Congo, Dem. Rep.",
  NA,
  "Czech Republic",
  "Egypt, Arab Rep.",
  "Guyana",
  NA,
  "Iran, Islamic Rep.",
  "Korea, Rep.",
  NA,
  NA,
  "Russian Federation",
  "Slovak Republic",
  NA,
  "United States"
)
cov19_dead$`Country Name` <- stri_replace_all_fixed(cov19_dead$country, patt, repl, vectorize_all = FALSE)
print("Beds - Unmatched countries (after renaming)")
print(setdiff(
  cov19_dead$`Country Name`,
  intersect(cov19_dead$`Country Name`, hosp_beds$`Country Name`)
))

###########################################################################################################################

tot_world_pop <-
  world_pop %>%
  group_by(Location) %>%
  summarise(population.1K = sum(PopMale + PopFemale))

print("World Pop - Unmatched countries (before renaming)")
print(setdiff(
  cov19_dead$`Country Name`,
  intersect(cov19_dead$`Country Name`, tot_world_pop$Location)
))
patt <- c(
  "Bolivia (Plurinational State of)",
  "Congo",
  "Côte d'Ivoire",
  "Czechia",
  "Egypt",
  "Iran (Islamic Republic of)",
  "Republic of Korea",
  "Republic of Moldova",
  "Slovakia",
  "United States of America",
  "Viet Nam"
)
repl <- c(
  "Bolivia",
  "Congo, Dem. Rep.",
  "Cote d'Ivoire",
  "Czech Republic",
  "Egypt, Arab Rep.",
  "Iran, Islamic Rep.",
  "Korea, Rep.",
  "Moldova",
  "Slovak Republic",
  "United States",
  "Vietnam"
)
tot_world_pop$`Country Name` <- stri_replace_all_fixed(tot_world_pop$Location, patt, repl, vectorize_all = FALSE)
print("World Population - Unmatched countries (after renaming)")
print(setdiff(
  cov19_dead$`Country Name`,
  intersect(cov19_dead$`Country Name`, tot_world_pop$`Country Name`)
))

###########################################################################################################################

hosp_beds_max <-
  hosp_beds %>%
  select(-`Country Code`, -`Indicator Name`, -`Indicator Code`) %>%
  gather(year, beds.year, -`Country Name`) %>%
  group_by(`Country Name`) %>%
  summarise(beds.1K = max(beds.year, na.rm = TRUE)) %>%
  filter(! is.infinite(beds.1K))

beds_tot <-
  inner_join(hosp_beds_max, tot_world_pop) %>%
  mutate(beds.1M = as.character(beds.1K * population.1K / 1E6)) %>%
  select(`Country Name`, beds.1M)

beds_death <-
  inner_join(beds_tot, cov19_dead) %>%
  mutate_if(is.double, function(d) return(d/as.numeric(.$beds.1M))) %>%
  select(-Location, -`Country Name`, -beds.1M) %>%
  gather(date.str, deaths, -country) %>%
  filter(deaths > 1.0) %>%
  filter(! is.infinite(deaths)) %>%
  mutate(dt = mdy(date.str)) %>%
  group_by(country) %>%
  arrange(dt) %>%
  mutate(day.number = dplyr::row_number())

gg <-
  ggplot(beds_death, aes(x = day.number, y = deaths, colour = country)) +
  geom_line() +
  scale_y_log10() +
  xlab("Day number") +
  ylab("Deaths per 1M hospital beds") +
  geom_dl(aes(label = country), method = list(dl.combine("last.points"), rot=-30, cex=0.7))

print(gg)
