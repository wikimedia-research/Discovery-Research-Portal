library(magrittr)
library(tidyr)
import::from(dplyr, group_by, summarize, ungroup, arrange, mutate, rename, select, left_join, keep_where = filter)
library(ggplot2)
library(cowplot)
import::from(ggthemes, tufte = theme_tufte)

options(scipen = 500)

deployments <- readr::read_csv("data/portal_deployments.csv") %>% arrange(Date)
deployments$Order <- 1:nrow(deployments)

my_theme <- function() {
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
    theme(panel.grid = element_line(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(color = "gray60"),
          strip.background = element_rect(fill = "gray40", color = "gray40"),
          strip.text = element_text(color = "white"),
          panel.border = element_rect(color = "gray40", fill = NA))
}

## Pageviews

pageviews <- dplyr::left_join(readr::read_csv("data/pageviews_all_all.csv"),
                              readr::read_csv("data/portal_pageviews.csv"),
                              by = "Date") %>% keep_where(Date >= "2015-11-20")

{ pageviews %>%
  gather(Site, Pageviews, -Date) %>%
  ggplot(aes(x = Date, y = Pageviews, color = Site)) +
  geom_line(alpha = 0.6) +
  geom_smooth(method = "gam", se = FALSE, formula = y ~ s(x, k = 20)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_log10(breaks = union(c(1e6, 1e7, 1.5e7, 2e7, 3e7, 7e7), seq(1e6, 3.5e8, 5e7)),
                labels = polloi::compress) +
  my_theme() + theme(legend.position = "bottom") +
  ggtitle("Pageviews") +
  geom_vline(data = deployments, aes(xintercept = as.numeric(Date)),
             linetype = "dashed") +
  geom_label(data = deployments, aes(x = Date, y = 5.1e7 + (Order-1) * 5e7, label = Deployment),
             color = "black", nudge_y = 0.05, fill = "white") +
  geom_point(data = deployments, aes(x = Date, y = 5.1e7 + (Order-1) * 5e7),
             color = "black") +
  scale_color_discrete(limits = c("en.wikipedia.org", "de.wikipedia.org", "ru.wikipedia.org", "fr.wikipedia.org", "wikipedia.org"))
  } %>% ggsave("pageviews.png", plot = ., path = "figures",
               units = "in", width = 15, height = 9, dpi = 300)

## Daily Aggregates

el_daily <- readr::read_csv("data/el_daily_aggregates.csv") %>% keep_where(Date >= "2015-11-20")
el_daily$Date %<>% as.Date

{ el_daily %>%
  gather(type, count, -Date) %>%
  ggplot(aes(x = Date, y = count)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid.major = element_line(color = "gray80"), panel.grid.major.y = element_blank()) +
  geom_line() +
  my_theme() +
  theme(panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor.y = element_line(color = "gray90")) +
  facet_wrap(~type, nrow = 2) +
  ggtitle("Number of sampled sessions (visitors) and visits over time",
          subtitle = "Users were selected for Event Logging at a sampling rate of 1 in 200") +
  geom_smooth(method = "gam", se = FALSE, formula = y ~ s(x, k = 20), color = RColorBrewer::brewer.pal(3, "Set1")[1], linetype = "dashed") +
  geom_smooth(method = "gam", se = FALSE, formula = y ~ s(x, k = 11), color = RColorBrewer::brewer.pal(3, "Set1")[2]) +
  geom_vline(data = deployments, aes(xintercept = as.numeric(Date)),
             linetype = "dashed") +
  geom_label(data = deployments, aes(x = Date, y = 1e3*Order, label = Deployment),
             color = "black", fill = "white")
} %>% ggsave("daily_aggregates.png", plot = ., path = "figures",
             units = "in", width = 20, height = 10, dpi = 300)

el_daily_us <- readr::read_csv("data/el_daily_aggregates_us.csv") %>% keep_where(Date >= "2015-11-20")
el_daily_us$Date %<>% as.Date

{ el_daily_us %>%
  # mutate(Country = ifelse(is.na(Country), "(Not Available)", Country)) %>%
  ggplot(aes(x = Date, y = sessions, fill = Country)) +
  geom_area(position = "fill", color = "black") +
  scale_y_continuous("Proportion of sessions", labels = scales::percent_format()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ggtitle("Proportion of sessions from US vs Non-US",
          subtitle = "Gray refers to sessions where a country could not be determined.") +
  my_theme() +
  geom_vline(data = deployments, aes(xintercept = as.numeric(Date)),
             linetype = "dashed") +
  geom_label(data = deployments, aes(x = Date, y = 0.1*Order, label = Deployment),
             color = "black", fill = "white") +
  geom_hline(yintercept = 0.5, color = "white", linetype = "dashed") +
  theme(legend.position = "bottom") } %>%
  ggsave("daily_aggregates_us.png", plot = ., path = "figures",
         units = "in", width = 15, height = 9, dpi = 300)

## Clickthrough Rate

el_ctr <- readr::read_csv("data/el_daily_ctr.csv") %>% keep_where(Date >= "2015-11-20")
el_ctr$Date %<>% as.Date

el_ctr %>%
  ggplot(aes(x = Date, y = ctr)) +
  geom_line(alpha = 0.4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous("Clickthrough Rate",
                     labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.1)) +
  geom_smooth(method = "gam", se = FALSE, formula = y ~ s(x, k = 5)) +
  geom_vline(data = deployments, aes(xintercept = as.numeric(Date)),
             linetype = "dashed") +
  geom_label(data = deployments, aes(x = Date, y = 0.1*Order + 0.6, label = Deployment),
             color = "black", fill = "white") +
  ggtitle("% of sessions with a clickthrough") +
  my_theme()

el_ctr_1st <- readr::read_csv("data/el_daily_ctr_1stvisit.csv") %>% keep_where(Date >= "2015-11-20")
el_ctr_1st$Date %<>% as.Date

el_ctr_1st %>%
  ggplot(aes(x = Date, y = ctr)) +
  geom_line(alpha = 0.4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous("Clickthrough Rate",
                     labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.1)) +
  geom_smooth(method = "gam", se = FALSE, formula = y ~ s(x, k = 5)) +
  geom_vline(data = deployments, aes(xintercept = as.numeric(Date)),
             linetype = "dashed") +
  geom_label(data = deployments, aes(x = Date, y = 0.1*Order + 0.6, label = Deployment),
             color = "black", fill = "white") +
  ggtitle("Daily Clickthrough Rate on First Visit",
          subtitle = "By Session") +
  my_theme()

## Actions Breakdown

el_daily_breakdown <- readr::read_csv("data/el_daily_click_breakdown.csv") %>% keep_where(Date >= "2015-11-20")
el_daily_breakdown$Date %<>% as.Date

el_daily_breakdown %>%
  rename(Section = section_used) %>%
  keep_where(!Section %in% c("language search", "other languages")) %>%
  ggplot(aes(x = Date, y = proportion, color = Section)) +
  geom_line(alpha = 0.4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.1)) +
  geom_smooth(method = "gam", se = FALSE, formula = y ~ s(x, k = 4)) +
  geom_vline(data = deployments, aes(xintercept = as.numeric(Date)),
             linetype = "dashed") +
  geom_label(data = deployments, aes(x = Date, y = 0.1*Order + 0.75, label = Deployment),
             color = "black", fill = "white") +
  ggtitle("Daily Action Breakdown",
          subtitle = "Unfiltered (overall) counts") +
  my_theme() +
  theme(legend.position = "bottom")

el_daily_breakdown_filtered <- readr::read_csv("data/el_daily_click_breakdown_filtered.csv") %>% keep_where(Date >= "2015-11-20")
el_daily_breakdown_filtered$Date %<>% as.Date

{
  el_daily_breakdown_filtered %>%
  rename(Section = section_used) %>%
  keep_where(!Section %in% c("language search", "other languages")) %>%
  ggplot(aes(x = Date, y = proportion, color = Section)) +
  geom_line(alpha = 0.55) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous("Proportion of sessions",
                     labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.1)) +
  geom_smooth(method = "gam", se = FALSE, formula = y ~ s(x, k = 4)) +
  ggtitle("Daily Action Breakdown",
          subtitle = "Filtered (\"last action taken\") counts like on Portal Dashboard") +
  my_theme() +
  theme(legend.position = "bottom") +
  theme(panel.grid.major.y = element_line(color = "gray80"),
          panel.grid.minor.y = element_line(color = "gray90")) +
  geom_vline(data = deployments, aes(xintercept = as.numeric(Date)),
             linetype = "dashed") +
  geom_text(data = deployments, aes(x = Date, y = 0.05, label = Deployment), color = "black",
            angle = 90, hjust = "left", vjust = "bottom", nudge_x = -2, size = 3.5) +
  scale_color_discrete(limits = c("no action", "search", "primary links", "secondary links", "other projects"))
} %>% cowplot::ggsave("action_breakdown.png", plot = ., path = "figures",
                      units = "in", width = 12, height = 6, dpi = 300)

el_daily_common <- readr::read_csv("data/el_daily_most_common.csv") %>% keep_where(Date >= "2015-11-20")
el_daily_common$Date %<>% as.Date

{
  el_daily_common %>%
  rename(Section = section_used) %>%
  keep_where(!Section %in% c("language search", "other languages")) %>%
  ggplot(aes(x = Date, y = proportion, color = Section)) +
  geom_line(alpha = 0.55) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous("Proportion of sessions (that had a clickthrough)",
                     labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.1)) +
  geom_smooth(method = "gam", se = FALSE, formula = y ~ s(x, k = 4)) +
  ggtitle("Daily Action Breakdown (Most common section clicked per visit)",
          subtitle = "Proportion of sessions where each section was the most commonly clicked one") +
  my_theme() +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor.y = element_line(color = "gray90")) +
  geom_vline(data = deployments, aes(xintercept = as.numeric(Date)),
             linetype = "dashed") +
  geom_text(data = deployments, aes(x = Date, y = 0.05, label = Deployment), color = "black",
            angle = 90, hjust = "left", vjust = "bottom", nudge_x = -2, size = 3.5) +
  scale_color_discrete(limits = c("search", "primary links", "secondary links", "other projects"))
} %>% cowplot::ggsave("most_common.png", plot = ., path = "figures",
                      units = "in", width = 12, height = 6, dpi = 300)
