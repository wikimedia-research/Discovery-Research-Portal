events <- readr::read_tsv("data/portal_events.tsv.gz")
# 2016-03-01 is duplicated :(
events <- events[!duplicated(events), ]

library(dplyr)
library(ggplot2)

clickthroughs <- events %>%
  group_by(date, session) %>%
  summarize(clickthrough = all(c("landing", "clickthrough") %in% type),
            clickthru_naive = "clickthrough" %in% type) %>%
  mutate(inconsistency = clickthrough != clickthru_naive) %>%
  summarize(clickthrough = sum(clickthrough)/length(clickthrough),
            `naive clickthrough` = sum(clickthru_naive)/length(clickthru_naive),
            `inconsistency rate` = sum(inconsistency)/length(inconsistency)) %>%
  mutate(portal = ifelse(date < "2016-03-10", "old", "new"))

readr::write_csv(clickthroughs, "data/clickthroughs_all.csv")

ggplot(data = clickthroughs, aes(x = date, color = portal)) +
  geom_line(aes(y = clickthrough)) +
  geom_line(aes(y = `naive clickthrough`), linetype = "dotted") +
  scale_x_datetime(date_breaks = "week", date_labels = "%a (%d %b)") +
  scale_y_continuous(labels = scales::percent_format())

search_clickthroughs <- events %>%
  filter(section_used == "search" | type == "landing") %>%
  group_by(date, session) %>%
  summarize(clickthrough = "clickthrough" %in% type) %>%
  summarize(clickthrough = sum(clickthrough)/length(clickthrough)) %>%
  mutate(portal = ifelse(date < "2016-03-10", "old", "new"))

readr::write_csv(search_clickthroughs, "data/clickthroughs_search.csv")

ggplot(data = search_clickthroughs, aes(x = date, y = clickthrough, color = portal)) +
  geom_line() +
  scale_x_datetime(date_breaks = "week", date_labels = "%a (%d %b)") +
  scale_y_continuous(labels = scales::percent_format())

