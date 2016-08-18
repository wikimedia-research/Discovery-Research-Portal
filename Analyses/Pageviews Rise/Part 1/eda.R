library(data.table)
library(ggplot2)

dir.create("figures")

webrequests <- readr::read_rds("data/portal_webrequest_counts_20160617-20160817_refined.rds")
pageviews <- readr::read_rds("data/portal_pageviews.rds")

{
  ggplot(pageviews, aes(x = date, y = pageviews)) +
  geom_line() +
  scale_y_continuous(labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  geom_vline(xintercept = as.numeric(min(webrequests$date)),
             linetype = "dashed", color = "cornflowerblue") +
  geom_vline(xintercept = as.numeric(max(webrequests$date)-1),
             linetype = "dashed", color = "cornflowerblue") +
  ggtitle("Wikipedia.org Portal Pageviews in 2016",
          subtitle = "via data collection script in wikimedia/discovery/golden")
} %>% ggsave("dash_pageviews.png", plot = ., path = "figures",
             width = 8, height = 6, dpi = 150)

# {
#   ggplot(pageviews, aes(x = date, y = pageviews)) +
#   geom_line() +
#   scale_y_continuous(labels = polloi::compress) +
#   scale_x_date(date_breaks = "1 month", date_labels = "%B",
#                limits = range(webrequests$date)) +
#   ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
#   theme(panel.grid = element_line(color = "gray70"),
#         panel.grid.major = element_line(color = "gray60")) +
#   ggtitle(sprintf("Wikipedia.org Portal Pageviews (%s-%s)",
#                   as.character(min(webrequests$date), "%d %B"),
#                   as.character(max(webrequests$date), "%d %B")),
#           subtitle = "via data collection script in wikimedia/discovery/golden")
# } %>% ggsave("dash_pageviews_windowed.png", plot = ., path = "figures",
#              width = 8, height = 6, dpi = 150)
  
webrequests <- dplyr::bind_rows(webrequests,
                                webrequests[,
                                            list(agent_type = "spider + user",
                                                 web_requests = sum(web_requests)),
                                            by = c("date", "http_status", "referer_class", "http_message")])

{
  webrequests[,
              list(requests = sum(web_requests)),
              by = c("date", "agent_type")] %>%
  ggplot(aes(x = date, y = requests, color = agent_type)) +
  geom_line() +
  scale_y_continuous(labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  scale_color_brewer(limits = c("spider + user", "user", "spider"), palette = "Set1") +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Webrequests",
          subtitle = "Using agent_type from the refined Webrequest data in Hive")
} %>% ggsave("webrequests_by_agent.png", plot = ., path = "figures",
             width = 8, height = 6, dpi = 150)

{
  webrequests[,
              list(requests = sum(web_requests)),
              by = c("date", "agent_type", "http_message")] %>%
  ggplot(aes(x = date, y = requests, color = agent_type)) +
  geom_line() +
  facet_wrap(~http_message, nrow = 4, ncol = 3) +
  scale_y_continuous(labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  scale_color_brewer(limits = c("spider + user", "user", "spider"), palette = "Set1") +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60")) +
  ggtitle("Wikipedia.org Portal Webrequests by HTTP Status and Agent Type",
          subtitle = "Using agent_type from the refined Webrequest data in Hive")
} %>% ggsave("webrequests_by_status-agent.png", plot = ., path = "figures",
             width = 12, height = 9, dpi = 150)
