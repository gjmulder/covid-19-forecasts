library(tidyverse)
library(lubridate)
library(stringi)
library(directlabels)

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

# cov19_dead_age <-
#   read_csv("age_death_rate.csv") %>%
#   mutate(death.rate.china = death.rate.5year.china * age.range.proportion) %>%
#   mutate(death.rate.korea = death.rate.5year.korea * age.range.proportion) %>%
#   select(-death.rate.5year.china,
#          -death.rate.5year.korea,
#          -age.range.proportion)

old_age <-
  c("60-64",
    "65-69",
    "70-74",
    "75-79",
    "80-84",
    "85-89",
    "90-94",
    "95-99",
    "100+")

# cov19_dead_tot <-
#   cov19_dead_age %>%
#   select(-AgeGrp) %>%
#   colSums
#
# cov19_old_dead_prop <-
#   cov19_dead_age %>%
#   filter(AgeGrp %in% old_age) %>%
#   select(-AgeGrp) %>%
#   colSums / cov19_dead_tot
#
# cov19_young_dead_prop <-
#   cov19_dead_age %>%
#   filter(! AgeGrp %in% old_age) %>%
#   select(-AgeGrp) %>%
#   colSums / cov19_dead_tot

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

expand_china_provs <- function() {
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
      # print(paste0(china_prop_prov$Division[i], ", ", china_pop$AgeGrp[j]))
      china_prov_lst[[paste(i, j)]]  <- tibble(
        Location = paste0("China, ", china_prop_prov$Division[i]),
        AgeGrp = china_pop$AgeGrp[j],
        PopMale = china_pop$PopMale[j] * china_prop_prov$prop[i],
        PopFemale = china_pop$PopFemale[j] * china_prop_prov$prop[i]
      )
    }
  }

  expanded_pop <-
    bind_rows(china_prov_lst) %>%
    bind_rows(world_pop) %>%
    filter(Location != "China")
  return(expanded_pop)
}

expanded_world_pop <-
  expand_china_provs()

old_world_pop <-
  expanded_world_pop %>%
  filter(AgeGrp %in% old_age) %>%
  group_by(Location) %>%
  summarise(population.1M = sum(PopMale + PopFemale) / 1000)

###########################################################################################################################
#
# Rename countries for Covid data

cov19_dead_all %>%
  rename(country = `Country/Region`) %>%
  mutate(country = ifelse(
    country == "China",
    paste0(country, ", ", `Province/State`),
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

plot_deaths <- function(pop_death_subset, day_number, max_day_number) {
  gg <-
    ggplot(pop_death_subset,
           aes(x = day.number, y = deaths, colour = country)) +
    geom_smooth(se = FALSE, size = 0.75) +
    scale_y_log10(
      labels = c(0.01, 0.1, 1, 10, 100),
      breaks = c(0.01, 0.1, 1, 10, 100),
      limits = c(0.01, 400)
    ) +
    xlim(1, max_day_number + 10) +
    xlab("Day number") +
    ylab("Deaths per 1M population greater than 59 years of age (log scale)") +
    ggtitle(
      paste0(
        "Day #",
        day_number,
        " - Covid-19 deaths per 1M maximum-risk (as of ",
        today() - 1,
        ")"
      )
    ) +
    geom_dl(aes(label = country), method = list(dl.combine("last.points"))) +
    theme(legend.position = "none") +
    geom_hline(yintercept = 1.0) +
    annotate("text", max(pop_death_long$day.number) + 4, 1.15, label = "1 Covid-19 death per 1M maximum-risk people")
  return(gg)
}

pop_death_joined <-
  inner_join(old_world_pop, cov19_dead) %>%
  select(-Location)

china_rest <-
  paste0("China, ", setdiff(china_prov_pop$Division, c("Hubei")))
# paste0("China, ", setdiff(china_prov_pop$Division, c("Hubei", "Hainan", "Hong Kong")))

pop_death_china_rest <-
  pop_death_joined %>%
  filter(country %in% china_rest) %>%
  select(-country) %>%
  summarise_all(sum) %>%
  mutate(country = "China, Rest of")

exclude_countries <-
  c("Iraq", "Philippines", "Netherlands", "Switzerland")

pop_death_long <-
  pop_death_joined %>%
  filter(.[, ncol(.)] > 3) %>%
  filter(!country %in% china_rest) %>%
  bind_rows(pop_death_china_rest) %>%
  mutate_if(is.double, function(d)
    return(d / as.integer(.$population.1M))) %>%
  select(-population.1M) %>%
  filter(!country %in% exclude_countries) %>%
  mutate(country = ifelse(country == "US", "United States of America", country)) %>%
  mutate(country = paste0("  ", country)) %>%
  gather(date.str, deaths, -country) %>%
  filter(!is.infinite(deaths)) %>%
  filter(deaths > 0.001) %>%
  mutate(dt = mdy(date.str)) %>%
  group_by(country) %>%
  arrange(dt) %>%
  mutate(day.number = row_number()) %>%
  filter(max(day.number) > 4)
# filter(day.number > 3)

if (interactive()) {
  print(plot_deaths(pop_death_long, max(pop_death_long$day.number), max(pop_death_long$day.number)))
} else {
  start_day <-
    7
  for (day_number in start_day:max(pop_death_long$day.number)) {
    print(paste0("Generating plot for day: ", day_number))
    gg <-
      plot_deaths(pop_death_long %>% filter(day.number < day_number), day_number, max(pop_death_long$day.number))
    ggsave(
      paste0(day_number - start_day + 1, "_deaths_norm_aged.png"),
      gg,
      width = 15,
      height = 10
    )
  }
}
