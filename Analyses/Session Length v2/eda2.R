###
# User Agents(OS, Browser)
# (UA parser generated a nasty list of device)
###

# Mobile tag
mobile_os <- c("Android","iOS","Windows Phone","BlackBerry OS")
sessions$mobile <- sessions$os %in% mobile_os

sessions %>%
group_by(mobile) %>%
summarize(clickthrough = sum(clickthrough), ctr = sum(clickthrough)/n(), n = n())

### Session Length

{ ggplot(keep_where(sessions, session_length > 0)) +
  geom_density(aes(x = session_length, fill = mobile), alpha = 0.5) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  scale_x_logtime(name = "Session Length") +
  ggtitle("Session lengths by Mobile OS",
          subtitle = "267K unique sessions from June and July 2016") +
  theme(legend.position = "bottom")} %>%
  ggsave("session_length_mobile.png", ., path = "figures", width = 8, height = 6, units = "in", dpi = 300)

top_os <- sessions %>%
group_by(os) %>%
summarize(sessions = n()) %>%
top_n(10, sessions) %>%
{ .$os }

sessions %>%
keep_where(os %in% top_os) %>%
group_by(os) %>%
summarize(clickthrough = sum(clickthrough), ctr = sum(clickthrough)/n(), n = n())

{ sessions %>%
keep_where(os %in% top_os, session_length > 0) %>%
ggplot(aes(y = session_length, x = os)) +
geom_hline(yintercept = 60 * 15, linetype = "dashed", color = "gray40") +
geom_violin(fill = "cornflowerblue") +
geom_boxplot(width = 0.25, outlier.shape = NA) +
scale_y_logtime(name = "Session length") +
scale_x_discrete(name = "OS", limits = rev(top_os)) +
coord_flip() +
ggtitle("Distribution of session lengths by OS",
        subtitle = "267K unique sessions from June and July 2016")} %>%
ggsave("session_length_by_os.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)


top_browser <- sessions %>%
group_by(browser) %>%
summarize(sessions = n()) %>%
top_n(10, sessions) %>%
{ .$browser }

sessions %>%
keep_where(browser %in% top_browser) %>%
group_by(browser) %>%
summarize(clickthrough = sum(clickthrough), ctr = sum(clickthrough)/n(), n = n())

{ sessions %>%
keep_where(browser %in% top_browser, session_length > 0) %>%
ggplot(aes(y = session_length, x = browser)) +
geom_hline(yintercept = 60 * 15, linetype = "dashed", color = "gray40") +
geom_violin(fill = "cornflowerblue") +
geom_boxplot(width = 0.25, outlier.shape = NA) +
scale_y_logtime(name = "Session length") +
scale_x_discrete(name = "OS", limits = rev(top_browser)) +
coord_flip() +
ggtitle("Distribution of session lengths by Browser",
        subtitle = "267K unique sessions from June and July 2016")} %>%
ggsave("session_length_by_browser.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)


{ sessions %>%
keep_where(session_length > 0) %>%
group_by(os) %>%
summarize(`median session length` = median(session_length, na.rm = TRUE),
        `median first visit session length` = median(session_length_first_visit, na.rm = TRUE),
        `median time to first click from first visit` = median(secs_to_first_click, na.rm = TRUE),
        n = n()) %>%
top_n(20, n) %>%
select(-n) %>%
gather(metric, seconds, -1) %>%
ggplot(aes(x = os, y = seconds)) +
geom_bar(stat = "identity", position = "dodge") +
geom_text(aes(label = paste0(seconds, "s")), nudge_y = 0.5, size = 3) +
coord_flip() +
facet_wrap(~metric, nrow = 3) +
theme(strip.text.x = element_text(size = 14)) +
ggtitle("Median session length by OS",
      subtitle = "267K unique sessions from June and July 2016; top 20 OS by # of sessions")} %>%
ggsave("session_lengths_medians_by_os.png", ., path = "figures", width = 12, height = 14, units = "in", dpi = 300)


{ sessions %>%
  keep_where(session_length > 0) %>%
  group_by(browser) %>%
  summarize(`median session length` = median(session_length, na.rm = TRUE),
            `median first visit session length` = median(session_length_first_visit, na.rm = TRUE),
            `median time to first click from first visit` = median(secs_to_first_click, na.rm = TRUE),
            n = n()) %>%
  top_n(20, n) %>%
  select(-n) %>%
  gather(metric, seconds, -1) %>%
  ggplot(aes(x = browser, y = seconds)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(seconds, "s")), nudge_y = 0.5, size = 3) +
  coord_flip() +
  facet_wrap(~metric, nrow = 3) +
  theme(strip.text.x = element_text(size = 14)) +
  ggtitle("Median session length by Browser",
          subtitle = "267K unique sessions from June and July 2016; top 20 Browser by # of sessions")} %>%
  ggsave("session_lengths_medians_by_browser.png", ., path = "figures", width = 12, height = 14, units = "in", dpi = 300)



temp <- sessions %>%
keep_where(session_length > 0) %>%
split(.$os) %>% 
map_df(function(df) {
  seconds <- seq(0, 60*15, 1)
  seshs <- as.data.frame(do.call(cbind, lapply(seconds, function(second) {
    return(sum(df$session_length > second))
  })))
  names(seshs) <- seconds
  return(cbind(os = head(df$os, 1), n = seshs$`0`[1], seshs[, -1]))
}) %>%
gather(seconds, sessions, -c(os, n)) %>%
mutate(seconds = as.numeric(seconds)) %>%
group_by(os, seconds) %>%
mutate(proportion = sessions/n) %>%
ungroup()
top_os <- top_n(dplyr::distinct(select(temp, os, n)), 20, n)$os
temp_top <- keep_where(temp, os %in% top_os)
gg_plots <- lapply(unique(temp_top$os), function(selected_os) {
temp_sub <- keep_where(temp_top, os == selected_os)
ggplot(data = temp_top, aes(x = seconds, y = proportion)) +
  geom_line(aes(group = os), color = "gray80") +
  geom_line(data = temp_sub, size = 1.25) +
  geom_vline(xintercept = temp_sub$seconds[which.min(abs(temp_sub$proportion - 0.5))[1]],
             linetype = "dashed") +
  scale_x_logtime() +
  scale_y_continuous("Proportion of sessions", labels = scales::percent_format()) +
  ggtitle(selected_os)
})
p <- plot_grid(plotlist = gg_plots, nrow = 5)
ggsave("survival_by_os.png", p, path = "figures", width = 16, height = 12, units = "in", dpi = 300)


temp <- sessions %>%
  keep_where(session_length > 0, !is.na(browser)) %>%
  split(.$browser) %>%
  map_df(function(df) {
    seconds <- seq(0, 60*15, 1)
    seshs <- as.data.frame(do.call(cbind, lapply(seconds, function(second) {
      return(sum(df$session_length > second))
    })))
    names(seshs) <- seconds
    return(cbind(browser = head(df$browser, 1), n = seshs$`0`[1], seshs[, -1]))
  }) %>%
  gather(seconds, sessions, -c(browser, n)) %>%
  mutate(seconds = as.numeric(seconds)) %>%
  group_by(browser, seconds) %>%
  mutate(proportion = sessions/n) %>%
  ungroup()
top_browser <- top_n(dplyr::distinct(select(temp, browser, n)), 20, n)$browser
temp_top <- keep_where(temp, browser %in% top_browser)
gg_plots <- lapply(unique(temp_top$browser), function(selected_browser) {
  temp_sub <- keep_where(temp_top, browser == selected_browser)
  ggplot(data = temp_top, aes(x = seconds, y = proportion)) +
    geom_line(aes(group = browser), color = "gray80") +
    geom_line(data = temp_sub, size = 1.25) +
    geom_vline(xintercept = temp_sub$seconds[which.min(abs(temp_sub$proportion - 0.5))[1]],
               linetype = "dashed") +
    scale_x_logtime() +
    scale_y_continuous("Proportion of sessions", labels = scales::percent_format()) +
    ggtitle(selected_browser)
})
p <- plot_grid(plotlist = gg_plots, nrow = 5)
ggsave("survival_by_browser.png", p, path = "figures", width = 16, height = 12, units = "in", dpi = 300)




### Time to first click from first visit

{ ggplot(keep_where(sessions, secs_to_first_click > 0)) +
  geom_density(aes(x = secs_to_first_click, fill = mobile), alpha = 0.5) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  scale_x_logtime(name = "Time") +
  ggtitle("Time to first clickthrough from initial landing by Mobile OS",
          subtitle = "242K unique sessions from June and July 2016") +
  theme(legend.position = "bottom")} %>%
  ggsave("first_clickthrough_mobile.png", ., path = "figures", width = 8, height = 6, units = "in", dpi = 300)


top_os <- sessions %>%
group_by(os) %>%
summarize(sessions = n()) %>%
top_n(10, sessions) %>%
{ .$os }

sessions %>%
keep_where(os %in% top_os) %>%
group_by(os) %>%
summarize(clickthrough = sum(clickthrough), ctr = sum(clickthrough)/n(), n = n())

{ sessions %>%
keep_where(os %in% top_os, secs_to_first_click>0) %>%
ggplot(aes(y = secs_to_first_click, x = os)) +
geom_hline(yintercept = 60 * 15, linetype = "dashed", color = "gray40") +
geom_violin(fill = "cornflowerblue") +
geom_boxplot(width = 0.25, outlier.shape = NA) +
scale_y_logtime(name = "Time to First Click from First Visit") +
scale_x_discrete(name = "OS", limits = rev(top_os)) +
coord_flip() +
ggtitle("Distribution of Time to First Click by OS",
        subtitle = "267K unique sessions from June and July 2016")} %>%
ggsave("time_to_first_click_by_os.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)


top_browser <- sessions %>%
group_by(browser) %>%
summarize(sessions = n()) %>%
top_n(10, sessions) %>%
{ .$browser }

sessions %>%
keep_where(browser %in% top_browser) %>%
group_by(browser) %>%
summarize(clickthrough = sum(clickthrough), ctr = sum(clickthrough)/n(), n = n())

{ sessions %>%
keep_where(browser %in% top_browser, secs_to_first_click>0) %>%
ggplot(aes(y = secs_to_first_click, x = browser)) +
geom_hline(yintercept = 60 * 15, linetype = "dashed", color = "gray40") +
geom_violin(fill = "cornflowerblue") +
geom_boxplot(width = 0.25, outlier.shape = NA) +
scale_y_logtime(name = "Time to First Click from First Visit") +
scale_x_discrete(name = "Browser", limits = rev(top_browser)) +
coord_flip() +
ggtitle("Distribution of Time to First Click by Browser",
      subtitle = "267K unique sessions from June and July 2016")} %>%
ggsave("time_to_first_click_by_browser.png", ., path = "figures", width = 10, height = 10, units = "in", dpi = 300)
