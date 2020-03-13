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

china_prov_pop <-
  read_csv("china-province-population.csv")


###########################################################################################################################
#
# Ratio deaths per old age

expand_china_provs <- function(world_pop) {
  china_prop_prov <-
    china_prov_pop %>%
    mutate(prop = Population / sum(.$Population))

  china_pop <-
    world_pop %>%
    filter(Location == "China")

  china_prov_lst <- list()
  for (i in 1:nrow(china_prop_prov)) {
    # print(china_prop_prov$Division[i])
    for (j in 1:nrow(china_pop)) {
      # print(paste0(china_prop_prov$Division[i], "-", china_pop$AgeGrp[j]))
      china_prov_lst[[paste(i, j)]] <-
        tibble(
          Location = paste0("China-", china_prop_prov$Division[i]),
          AgeGrp = china_pop$AgeGrp[j],
          PopMale = china_pop$PopMale[j] * china_prop_prov$prop[i],
          PopFemale = china_pop$PopFemale[j] * china_prop_prov$prop[i]
        )

    }
  }

  expanded_pop <-
    world_pop %>%
      filter(Location != "China") %>%
      bind_rows %>%
      bind_rows(china_prov_lst)
  return(expanded_pop)
}

expanded_world_pop <-
  world_pop %>%
  expand_china_provs


old_world_pop <-
  expanded_world_pop %>%
  filter(
    AgeGrp %in% c(
      "60-64",
      "65-69",
      "70-74",
      "75-79",
      "80-84",
      "85-89",
      "90-94",
      "95-99",
      "100+"
    )
  ) %>%
  group_by(Location) %>%
  summarise(population.1M = as.character(sum(PopMale + PopFemale) / 1000))

###########################################################################################################################
#
# Rename countries for Covid data

cov19_dead_all %>%
  rename(country = `Country/Region`) %>%
  mutate(country = ifelse(
    country == "China",
    paste0(country, "-", `Province/State`),
    country
  )) %>%
  select(-`Province/State`, -Lat, -Long) %>%
  group_by(country) %>%
  summarise_all(sum) ->
  cov19_dead

print("Age - Unmatched countries (before renaming)")
print(setdiff(
  cov19_dead$country,
  intersect(cov19_dead$country, expanded_world_pop$Location)
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
cov19_dead$Location <-
  stri_replace_all_fixed(cov19_dead$country, patt, repl, vectorize_all = FALSE)
print("Age - Unmatched countries (after renaming)")
print(setdiff(
  cov19_dead$Location,
  intersect(cov19_dead$Location, expanded_world_pop$Location)
))

###########################################################################################################################

pop_death <-
  inner_join(old_world_pop, cov19_dead) %>%
  mutate_if(is.double, function(d)
    return(d / as.integer(.$population.1M))) %>%
  select(-Location, -population.1M) %>%
  mutate(country = paste0("   ", country)) %>%
  gather(date.str, deaths, -country) %>%
  filter(!is.infinite(deaths)) %>%
  filter(deaths > 0.3) %>%
  mutate(dt = mdy(date.str)) %>%
  group_by(country) %>%
  arrange(dt) %>%
  mutate(day.number = dplyr::row_number())

gg <-
  ggplot(pop_death, aes(x = day.number, y = deaths, colour = country)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  xlab("Day number") +
  ylab("Deaths per 1M population greater than 59 years of age") +
  ggtitle("Covid-19 deaths per 1M older aged people (log scales)") +
  geom_dl(aes(label = country), method = list(dl.combine("last.points"), rot =
                                                -30, cex = 0.7))

print(gg)