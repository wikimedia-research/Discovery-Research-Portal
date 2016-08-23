library(tidyr)
import::from(dplyr, group_by, summarize, ungroup, mutate, rename, keep_where = filter, tbl_df, arrange, select, top_n)
library(purrr)
library(ggplot2)
library(cowplot)
import::from(ggthemes, theme_tufte)

sessions <- readr::read_rds("data/portal-session-lengths-refined.rds")

dir.create("figures")
theme_set(theme_tufte(base_family = "Gill Sans", base_size = 12))

logtime_breaks <- c(1, 2, 5, 10, 30, 60, 60*5, 60*10, 60*15, 60*30, 60*60, 60*60*24)
logtime_labels <- function(breaks) {
  lbls <- breaks %>%
    round %>%
    lubridate::seconds_to_period() %>%
    tolower %>%
    gsub(" ", "", .) %>%
    sub("(.*[a-z])0s$", "\\1", .) %>%
    sub("(.*[a-z])0m$", "\\1", .) %>%
    sub("(.*[a-z])0h$", "\\1", .)
  return(lbls)
}
scale_x_logtime <- function(...) {
  scale_x_log10(..., breaks = logtime_breaks, labels = logtime_labels)
}
scale_y_logtime <- function(...) {
  scale_y_log10(..., breaks = logtime_breaks, labels = logtime_labels)
}

{ ggplot(keep_where(sessions, session_length > 0)) +
  geom_density(aes(x = session_length), fill = "gray20") +
  scale_x_logtime(name = "Session Length") +
  ggtitle("Session length", subtitle = "267K unique sessions from June and July 2016")} %>%
  ggsave("session_length_overall.png", ., path = "figures", width = 8, height = 6, units = "in", dpi = 300)

{ ggplot(keep_where(sessions, session_length > 0)) +
  geom_density(aes(x = session_length, fill = clickthrough), alpha = 0.5) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  scale_x_logtime(name = "Session Length") +
  ggtitle("Session lengths by clickthrough (at some point in the session)",
          subtitle = "267K unique sessions from June and July 2016") +
  theme(legend.position = "bottom")} %>%
  ggsave("session_length_clickthrough.png", ., path = "figures", width = 8, height = 6, units = "in", dpi = 300)

{ sessions %>%
  keep_where(session_length > 0) %>%
  mutate(total_visits = ifelse(total_visits >= 20, "20+", total_visits)) %>%
  ggplot(aes(y = session_length, x = total_visits)) +
  geom_hline(yintercept = 60 * 15, linetype = "dashed", color = "gray40") +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = 0.25, outlier.shape = NA) +
  scale_y_logtime(name = "Session length") +
  scale_x_discrete(name = "Total visits per session", limits = c(1:19, "20+")) +
  ggtitle("Session lengths by number of visits",
          subtitle = "267K unique sessions from June and July 2016")} %>%
  ggsave("session_length_by_visits.png", ., path = "figures", width = 10, height = 12, units = "in", dpi = 300)

{ ggplot(keep_where(sessions, session_length_first_visit > 0)) +
  geom_density(aes(x = session_length_first_visit), fill = "gray20") +
  scale_x_logtime(name = "Time between first two events") +
  ggtitle("Distribution of time between first two events",
          subtitle = 'Roughly "first visit session length"; 266K unique sessions from June and July 2016')} %>%
  ggsave("first_visit_session_length.png", ., path = "figures", width = 8, height = 6, units = "in", dpi = 300)

{ ggplot(keep_where(sessions, session_length_first_visit > 0)) +
  geom_density(aes(x = session_length_first_visit, fill = clickthrough), alpha = 0.25) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  scale_x_logtime(name = "Session Length") +
  ggtitle("Distribution of time between first two events by clickthrough (at some point)",
          subtitle = 'Roughly "first visit session length"; 266K unique sessions from June and July 2016') +
  theme(legend.position = "bottom")} %>%
  ggsave("first_visit_session_length_by_clickthrough.png", ., path = "figures", width = 8, height = 6, units = "in", dpi = 300)

{ ggplot(keep_where(sessions, secs_to_first_click > 0)) +
  geom_density(aes(x = secs_to_first_click), fill = "gray20") +
  scale_x_logtime(name = "Time") +
  ggtitle("Time to first clickthrough from initial landing",
          subtitle = 'Roughly "first visit session length"; 242K unique sessions from June and July 2016')} %>%
  ggsave("first_clickthrough.png", ., path = "figures", width = 8, height = 6, units = "in", dpi = 300)

{ sessions %>%
  keep_where(session_length > 0, !is.na(country_name)) %>%
  group_by(country_name) %>%
  summarize(`median session length` = median(session_length, na.rm = TRUE),
            `median first visit session length` = median(session_length_first_visit, na.rm = TRUE),
            `median time to first click from first visit` = median(secs_to_first_click, na.rm = TRUE),
            n = n()) %>%
  top_n(20, n) %>%
  select(-n) %>%
  gather(metric, seconds, -1) %>%
  ggplot(aes(x = country_name, y = seconds)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(seconds, "s")), nudge_y = 0.5, size = 3) +
  coord_flip() +
  facet_wrap(~metric, nrow = 3) +
  theme(strip.text.x = element_text(size = 14)) +
  ggtitle("Median session length by country",
          subtitle = "170K unique sessions from June and July 2016; top 20 countries by # of sessions")} %>%
  ggsave("session_lengths_medians_by_country.png", ., path = "figures", width = 12, height = 14, units = "in", dpi = 300)

temp <- sessions %>%
  keep_where(session_length > 0, !is.na(country_name)) %>%
  # keep_where(country == "US") %>% head(100) %>%
  split(.$country_name) %>% # df <- df$`United States`
  map_df(function(df) {
    seconds <- seq(0, 60*15, 1)
    seshs <- as.data.frame(do.call(cbind, lapply(seconds, function(second) {
      return(sum(df$session_length > second))
    })))
    names(seshs) <- seconds
    return(cbind(country_name = head(df$country_name, 1), n = seshs$`0`[1], seshs[, -1]))
  }) %>%
  gather(seconds, sessions, -c(country_name, n)) %>%
  mutate(seconds = as.numeric(seconds)) %>%
  group_by(country_name, seconds) %>% 
  mutate(proportion = sessions/n) %>%
  ungroup()
top_countries <- top_n(dplyr::distinct(select(temp, country_name, n)), 20, n)$country_name
temp_top <- keep_where(temp, country_name %in% top_countries)
gg_plots <- lapply(unique(temp_top$country_name), function(selected_country) {
  temp_sub <- keep_where(temp_top, country_name == selected_country)
  ggplot(data = temp_top, aes(x = seconds, y = proportion)) +
    geom_line(aes(group = country_name), color = "gray80") +
    geom_line(data = temp_sub, size = 1.25) +
    geom_vline(xintercept = temp_sub$seconds[which.min(abs(temp_sub$proportion - 0.5))[1]],
               linetype = "dashed") +
    scale_x_logtime() +
    scale_y_continuous("Proportion of sessions", labels = scales::percent_format()) +
    ggtitle(selected_country)
})
p <- plot_grid(plotlist = gg_plots, nrow = 5)
ggsave("survival_by_country.png", p, path = "figures", width = 16, height = 12, units = "in", dpi = 300)

# readr::write_rds(temp, "data/temp.rds", "gz")

# library(ggrepel)
library(mgcv)

# theme_set(ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 12))
top_countries <- top_n(dplyr::distinct(select(temp, country_name, n)), 100, n)$country_name
p <- ggplot(data = keep_where(temp, country_name %in% top_countries),
            aes(x = seconds, y = proportion)) +
  # Make the survival curves semi-transparent
  geom_line(aes(group = country_name), alpha = 0.1) +
  # Highlight the United States survival curve
  geom_line(data = keep_where(temp, country_name == "United States"), color = "#e41a1c", size = 1.25) +
  # Add the annotation for the highlighted U.S. survival curve
  geom_text(data = keep_where(temp, country_name == "United States", seconds == 10),
                  aes(label = sprintf("%.0f%% of US sessions\nlasted longer than 10s",
                                      100*proportion)),
            hjust = "right", vjust = "top", color = "#e41a1c", nudge_x = -0.1, nudge_y = -0.025) +
  # Horizontal line indicating the proportion of sessions lasting longer than 10s
  geom_segment(data = keep_where(temp, country_name == "United States", seconds == 10),
               aes(x = seconds - 10, y = proportion,
                   xend = seconds, yend = proportion),
               color = "#e41a1c", linetype = "dashed") +
  # Vertical arrow pointing at the highlighted survival curve at 10s
  geom_segment(data = keep_where(temp, country_name == "United States", seconds == 10),
               aes(y = 0, yend = proportion - 0.05, x = seconds, xend = seconds),
               color = "#e41a1c", arrow = arrow(length = unit(0.1, "in"), type = "closed")) +
  # GAM-smoothed median
  geom_smooth(data = summarize(group_by(temp, seconds), proportion = median(proportion)),
              se = FALSE, method = "gam", formula = y ~ s(x, k = 12), color = "black", size = 1.25) +
  # Rescale the time axis
  scale_x_logtime(name = "T (session length)") +
  # Use reader-friendly formatting on the proportion axis
  scale_y_continuous("Proportion of sessions longer than T", labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.1)) +
  # Currently only the dev version of ggplot2 includes Bob Rudis' subtitles
  # > devtools::install_github("hadley/ggplot2")
  ggtitle("Proportion of sessions longer than T length of time",
          subtitle = paste(polloi::compress(sum(dplyr::distinct(select(temp, country_name, n))$n), 1),
                           "sessions; top 100 countries by volume of sessions; black curve is smoothed median"))
ggsave("survival_countries.png", p, path = "figures", width = 8, height = 6, units = "in", dpi = 300)

temp <- sessions %>%
  keep_where(session_length > 0, !is.na(country_name)) %>%
  split(.$primary_language) %>%
  map_df(function(df) {
    seconds <- seq(0, 60*15, 1)
    seshs <- as.data.frame(do.call(cbind, lapply(seconds, function(second) {
      return(sum(df$session_length > second))
    })))
    names(seshs) <- seconds
    return(cbind(primary_language = head(df$primary_language, 1), n = seshs$`0`[1], seshs[, -1]))
  }) %>%
  gather(seconds, sessions, -c(primary_language, n)) %>%
  mutate(seconds = as.numeric(seconds)) %>%
  group_by(primary_language, seconds) %>%
  mutate(proportion = sessions/n) %>%
  ungroup()

top_languages <- top_n(dplyr::distinct(select(temp, primary_language, n)), 30, n)$primary_language
p <- ggplot(data = keep_where(temp, primary_language %in% top_languages),
            aes(x = seconds, y = proportion)) +
  # Make the survival curves semi-transparent
  geom_line(aes(group = primary_language), alpha = 0.15) +
  # Highlight the Russian survival curve
  geom_line(data = keep_where(temp, primary_language == "Russian"), color = "#e41a1c", size = 1.25) +
  # Add the annotation for the highlighted Eng. survival curve
  geom_text(data = keep_where(temp, primary_language == "Russian", seconds == 10),
            aes(label = sprintf("%.0f%% of Russian-using\nsessions lasted longer than 10s",
                                100*proportion)),
            hjust = "left", vjust = "top", color = "#e41a1c", nudge_x = 0.2, nudge_y = 0.05) +
  # Horizontal line indicating the proportion of sessions lasting longer than 10s
  geom_segment(data = keep_where(temp, primary_language == "Russian", seconds == 10),
               aes(x = seconds - 10, y = proportion,
                   xend = seconds, yend = proportion),
               color = "#e41a1c", linetype = "dashed") +
  # Highlight the English survival curve
  geom_line(data = keep_where(temp, primary_language == "English"), color = "#377eb8", size = 1.25) +
  # Add the annotation for the highlighted Eng. survival curve
  geom_text(data = keep_where(temp, primary_language == "English", seconds == 10),
            aes(label = sprintf("%.0f%% of Eng. sessions\nlasted longer than 10s",
                                100*proportion)),
            hjust = "right", vjust = "top", color = "#377eb8", nudge_x = -0.1, nudge_y = -0.025) +
  # Horizontal line indicating the proportion of sessions lasting longer than 10s
  geom_segment(data = keep_where(temp, primary_language == "English", seconds == 10),
               aes(x = seconds - 10, y = proportion,
                   xend = seconds, yend = proportion),
               color = "#377eb8", linetype = "dashed") +
  # GAM-smoothed median
  geom_smooth(data = summarize(group_by(temp, seconds), proportion = median(proportion)),
              se = FALSE, method = "gam", formula = y ~ s(x, k = 12), color = "black", size = 1.25) +
  # Rescale the time axis
  scale_x_logtime(name = "T (session length)") +
  # Use reader-friendly formatting on the proportion axis
  scale_y_continuous("Proportion of sessions longer than T", labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.1)) +
  # Currently only the dev version of ggplot2 includes Bob Rudis' subtitles
  # > devtools::install_github("hadley/ggplot2")
  ggtitle('Session "survival" curve by most preferred language',
          subtitle = paste(polloi::compress(sum(dplyr::distinct(select(temp, primary_language, n))$n), 1),
                           "sessions; top 30 languages by volume of sessions; black curve is smoothed median"))
ggsave("survival_languages.png", p, path = "figures", width = 8, height = 6, units = "in", dpi = 300)

top_referrers <- sessions %>%
  # keep_where(!is.na(most_common_referrer)) %>%
  mutate(most_common_referrer = ifelse(is.na(most_common_referrer), "direct (not referred)", most_common_referrer)) %>%
  group_by(most_common_referrer) %>%
  summarize(sessions = n()) %>%
  top_n(11, sessions) %>%
  { .$most_common_referrer }

sessions %>%
  mutate(most_common_referrer = ifelse(is.na(most_common_referrer), "direct (not referred)", most_common_referrer)) %>%
  keep_where(most_common_referrer %in% top_referrers) %>%
  group_by(most_common_referrer) %>%
  summarize(clickthrough = sum(clickthrough), ctr = sum(clickthrough)/n(), n = n())

{ sessions %>%
  mutate(most_common_referrer = ifelse(is.na(most_common_referrer), "direct (not referred)", most_common_referrer)) %>%
  keep_where(most_common_referrer %in% top_referrers, session_length > 0) %>%
  ggplot(aes(y = session_length, x = most_common_referrer)) +
  geom_hline(yintercept = 60 * 15, linetype = "dashed", color = "gray40") +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = 0.25, outlier.shape = NA) +
  scale_y_logtime(name = "Session length") +
  scale_x_discrete(name = "Referrer", limits = rev(union(c("direct (not referred)", "google", "bing", "facebook", "baidu.com"), top_referrers))) +
  coord_flip() +
  ggtitle("Distribution of session lengths by referrer",
          subtitle = "39K unique referred and 391K non-referred sessions from June and July 2016")} %>%
  ggsave("session_length_by_referrer.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)
