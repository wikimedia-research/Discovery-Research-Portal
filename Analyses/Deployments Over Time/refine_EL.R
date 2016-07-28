library(magrittr)
library(tidyr)
import::from(dplyr, group_by, summarize, ungroup, arrange, mutate, rename, select, left_join, keep_where = filter)
import::from(dplyr, tally, top_n)

events <- readr::read_rds("data/portal-T138397-data.rds")

events %>%
  group_by(Date = date, identity) %>%
  summarize(visits = max(visit)) %>%
  summarize(sessions = n(), visits = sum(visits)) %>%
  readr::write_csv("data/el_daily_aggregates.csv")

events %>%
  group_by(Date = date, identity) %>%
  summarize(clickthrough = any(type == "clickthrough")) %>%
  summarize(ctr = round(sum(clickthrough)/n(), 4)) %>%
  readr::write_csv("data/el_daily_ctr.csv")

events %>%
  keep_where(visit == 1) %>%
  group_by(Date = date, identity) %>%
  summarize(clickthrough = any(type == "clickthrough")) %>%
  summarize(ctr = round(sum(clickthrough)/n(), 4)) %>%
  readr::write_csv("data/el_daily_ctr_1stvisit.csv")

# Most commonly clicked section per visit
events %>%
  keep_where(type == "clickthrough") %>%
  group_by(Date = date, identity, visit, section_used) %>%
  tally %>%
  top_n(1, n) %>%
  ungroup %>%
  group_by(Date, section_used) %>%
  tally %>%
  mutate(proportion = round(nn/sum(nn), 4)) %>%
  ungroup %>%
  readr::write_csv("data/el_daily_most_common.csv")

# Most common section clicked on first visit
events %>%
  keep_where(visit == 1, type == "clickthrough") %>%
  group_by(Date = date, identity, section_used) %>%
  tally %>%
  top_n(1, n) %>%
  ungroup %>%
  group_by(Date, section_used) %>%
  tally %>%
  mutate(proportion = round(nn/sum(nn), 4)) %>%
  ungroup %>%
  readr::write_csv("data/el_daily_most_common_1stvisit.csv")

# Raw proportions of actions
events %>%
  mutate(section_used = ifelse(is.na(section_used), "no action", section_used)) %>%
  group_by(Date = date, section_used) %>%
  tally %>%
  mutate(proportion = round(n/sum(n), 4)) %>%
  ungroup %>%
  readr::write_csv("data/el_daily_click_breakdown.csv")

# Raw proportions of actions (filtered)
events %>%
  mutate(section_used = ifelse(is.na(section_used), "no action", section_used)) %>%
  group_by(Date = date, identity) %>%
  arrange(ts) %>%
  summarize(section_used = tail(section_used, 1)) %>%
  group_by(Date, section_used) %>%
  tally %>%
  mutate(proportion = round(n/sum(n), 4)) %>%
  ungroup %>%
  readr::write_csv("data/el_daily_click_breakdown_filtered.csv")

# US Traffic vs Non-US Traffic
events %>%
  group_by(Date = date, Country = ifelse(country == "US", "US", "Non-US"), identity) %>%
  summarize(visits = max(visit)) %>%
  group_by(Date, Country) %>%
  summarize(sessions = length(unique(identity)), visits = sum(visits)) %>%
  ungroup %>%
  readr::write_csv("data/el_daily_aggregates_us.csv")

# Sites visited from the Wikipedia Portal (by section)
events %>%
  keep_where(!is.na(destination)) %>%
  keep_where(destination != "search-redirect.php") %>%
  group_by(Date = date, section_used, destination) %>%
  tally() %>%
  spread(section_used, n, fill = 0) %>%
  ungroup %>%
  readr::write_csv("data/el_destinations.csv")
