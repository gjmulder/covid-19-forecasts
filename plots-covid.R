library(tidyverse)
library(lubridate)
library(stringi)
library(directlabels)
library(forecast)
library( ggpubr)

jhu_url_base <-
  paste0(
    "https://raw.githubusercontent.com/CSSEGISandData/",
    "COVID-19/master/csse_covid_19_data/",
    "csse_covid_19_time_series/"
  )

cov19_confirmed_all <-
  read_csv(paste0(jhu_url_base, "time_series_19-covid-Confirmed.csv"))

# cov19_dead_all <-
#   read_csv(paste0(jhu_url_base, "time_series_19-covid-Deaths.csv"))

# cov19_recovered_all <-
#   read_csv(paste0(jhu_url_base, "time_series_19-covid-Recovered.csv"))


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
    j <- 1
      # print(paste0(china_prop_prov$Division[i], "-", china_pop$AgeGrp[j]))
      china_prov_lst[[paste(i, j)]]  <- tibble(
        Location = paste0("China-", china_prop_prov$Division[i]),
        AgeGrp = china_pop$AgeGrp[j],
        PopMale = china_pop$PopMale[j] * china_prop_prov$prop[i],
        PopFemale = china_pop$PopFemale[j] * china_prop_prov$prop[i]
      )
  }

  expanded_pop <-
    bind_rows(china_prov_lst) %>%
    bind_rows(world_pop) %>%
    filter(Location != "China")
  return(expanded_pop)
}

expanded_world_pop <-
  expand_china_provs()

tot_world_pop <-
  expanded_world_pop %>%
  group_by(Location) %>%
  summarise(population.1M = sum(PopMale + PopFemale) / 1000)

###########################################################################################################################
#
# Rename countries for Covid data

cov19_confirmed <-
  cov19_confirmed_all %>%
  rename(country = `Country/Region`) %>%
  mutate(country = ifelse(
    country == "China",
    paste0(country, "-", `Province/State`),
    country
  )) %>%
  select(-`Province/State`, -Lat, -Long) %>%
  group_by(country) %>%
  summarise_all(sum)

print("Population - Unmatched countries (before renaming)")
print(setdiff(
  cov19_confirmed$country,
  intersect(cov19_confirmed$country, expanded_world_pop$Location)
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
cov19_confirmed$Location <-
  stri_replace_all_fixed(cov19_confirmed$country, patt, repl, vectorize_all = FALSE)
print("Population - Unmatched countries (after renaming)")
print(setdiff(
  cov19_confirmed$Location,
  intersect(cov19_confirmed$Location, expanded_world_pop$Location)
))

###########################################################################################################################

pop_confirmed_joined <-
  inner_join(tot_world_pop, cov19_confirmed) %>%
  select(-Location)

china_rest <-
  c("Singapore", "Korea, South", paste0("China-", china_prov_pop$Division))
  # setdiff(china_prov_pop$Division, c("Hubei")))
# paste0("China-", setdiff(china_prov_pop$Division, c("Hubei", "Hainan", "Hong Kong")))

# pop_confirmed_china_rest <-
#   pop_confirmed_joined %>%
#   filter(country %in% china_rest) %>%
#   select(-country) %>%
#   summarise_all(sum) %>%
#   mutate(country = "China-Rest")

pop_confirmed_long <-
  pop_confirmed_joined %>%
  filter(! country %in% china_rest) %>%
  # bind_rows(pop_confirmed_china_rest) %>%
  mutate_if(is.double, function(d)
    return(d / as.integer(.$population.1M))) %>%
  select(-population.1M) %>%
  mutate(country = paste0("   ", country)) %>%
  gather(date.str, confirmed, -country) %>%
  filter(!is.infinite(confirmed)) %>%
  filter(!is.nan(confirmed)) %>%
  filter(confirmed > 15.0) %>%
  mutate(dt = mdy(date.str)) %>%
  group_by(country) %>%
  arrange(dt) %>%
  mutate(day.number = dplyr::row_number()) %>%
  filter(day.number > 5)

conf_ts <-
  pop_confirmed_long %>%
  group_by(day.number) %>%
  summarise(confirmed = sum(confirmed))

fit <- ets(conf_ts$confirmed, lambda = "auto")
print(checkresiduals(fit))
fcast <- forecast(fit, h=14)
print(accuracy(fcast))
print(autoplot(fcast) + scale_y_log10())
print(fcast)

# model <- paste0(fit$components[1:3], collapse = '')
# damped <- fit$components[4] == "TRUE"
# alpha <- fit$par[1]
# beta <- fit$par[2]
# lambda <- fit$lambda
# conf_df <-
#   as_tibble(t(cov19_confirmed[, 2:(ncol(cov19_confirmed)-1)]))
# colnames(conf_df) <- cov19_confirmed$country

gg <-
  ggplot(pop_confirmed_long, aes(x = day.number, y = confirmed, colour = country)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  scale_y_log10() +
  # scale_x_log10() +
  xlab("Day number") +
  ylab("Confirmed per 1M population") +
  ggtitle("Covid-19 Confirmed per 1M people (log scales)") +
  geom_dl(aes(label = country), method = list(dl.combine("last.points"), rot =
                                                -30, cex = 0.7)) +
  guides(col = guide_legend(ncol = 1))
  # stat_regline_equation(
  #   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")),
  #   # label.x = max(mongo_plot_data$search.time)/2,
  #   # label.y = min_err,
  #   colour = country,
  #   na.rm = TRUE
  # )
print(gg)
ggsave("confirmed_norm_aged.png")
