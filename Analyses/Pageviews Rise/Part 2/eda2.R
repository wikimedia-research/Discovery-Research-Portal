# system2("scp", c("stat2:/home/bearloga/portal_webrequest_counts_by_ip-top10.tsv", "data/"))
# system2("scp", c("stat2:/home/bearloga/portal_webrequest_counts_by_ip-all.tsv", "data/"))

library(data.table)
library(magrittr)
library(ggplot2)

wdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

webrequest_counts <- as.data.table(readr::read_tsv("data/portal_webrequest_counts_by_ip-top10.tsv"))

library(extrafont) # install.packages("extrafont")
# font_import(paths = "/Library/Fonts/", pattern = "^.*\\.(ttf|otf|ttc)$")

{
  merge(webrequest_counts,
        webrequest_counts[, list(countries = paste(unique(country), collapse = ", ")), by = c("client_ip")],
        by = "client_ip") %>%
  dplyr::mutate(identity = sprintf("%s from %s", client_ip, countries),
                date = paste0(wdays[as.numeric(as.character(date, "%u"))], ", ", as.character(date, "%d %B"))) %>%
  dplyr::group_by(date, identity) %>%
  dplyr::summarize(pageviews = sum(pageviews)) %>%
  tidyr::spread(identity, pageviews, fill = 0) %>%
  tidyr::gather(identity, pageviews, -date) %>%
  ggplot(aes(x = factor(date), color = factor(date), y = pageviews)) +
  geom_pointrange(aes(ymin = 0, ymax = pageviews), size = 1.1) +
  facet_wrap(~ identity) +
  coord_flip() +
  scale_color_brewer("Date", palette = "Set1") +
  scale_x_discrete("Date", limits = rev(c("Saturday, 18 June", "Wednesday, 17 August"))) +
  scale_y_continuous(breaks = c(1, seq(1e4, 5e4, 1e4)), labels = polloi::compress, limits = c(0, 6e4)) +
  geom_text(aes(label = polloi::compress(pageviews), y = pageviews + 5e3, hjust = 0, vjust = 0.5), color = "black", size = 3) +
  ggthemes::theme_tufte(base_size = 8, base_family = "Verdana") +
  theme(legend.position = "bottom", panel.grid = element_line("gray70")) +
  ggtitle("Wikipedia.org Portal Pageviews (PVs) by IP Address",
          subtitle = "These 'top 10 IPs by PVs' account for about 326K-765K (1.8%-4.8%) of the 15.9M-18.4M PVs/day")
} %>% ggsave("top10.pdf", plot = ., path = "figures", width = 21, height = 7)

merge(webrequest_counts,
      webrequest_counts[, list(countries = paste(unique(country), collapse = ", ")), by = c("client_ip")],
      by = "client_ip") %>%
  dplyr::group_by(date, client_ip, countries) %>%
  dplyr::summarize(pageviews = sum(pageviews)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(countries, client_ip, date) %>%
  readr::write_tsv("data/portal_webrequest_counts_by_top10ips.tsv")

webrequest_counts <- as.data.table(dplyr::filter(readr::read_tsv("data/portal_webrequest_counts_by_ip-all.tsv"), date == "2016-08-17"))
{
  webrequest_counts %>%
  # dplyr::group_by(`less than 100 PVs` = pageviews < 100) %>%
  # dplyr::tally() %>%
  # dplyr::mutate(percent = sprintf("%.2f%%", 100*n/sum(n)))
  ggplot(aes(x = pageviews)) +
  geom_histogram(aes(y = cumsum(..count..)/sum(..count..)), binwidth = 0.05) +
  scale_x_log10("Number of Pageviews Per Day",
                breaks = c(1, seq(2, 10, 2), 25, 50, 100, 500, 1000, 1.5e4, 5e4)) +
  scale_y_continuous("Proportion of IPs", labels = scales::percent_format()) +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  ggtitle("Cumulative density histogram of Wikipedia.org Portal PVs on 17 Aug 2016",
          subtitle = "99.58% of the IPs have <100 PVs per day")
} %>% ggsave("distribution.png", plot = ., path = "figures", width = 12, height = 6, dpi = 150)

library(plotly) # install.packages("plotly")

plot_ly(x = webrequest_counts$pageviews, type = "histogram",
        font_family = "Open Sans") %>%
  layout(title = "Distribution of Wikipedia.org PVs Per IP Address on 17 Aug 2016",
         titlefont = list(
           family = "Open Sans, sans-serif",
           size = 14
         ),
         xaxis = list(
           title = "Pageviews from an IP address",
           titlefont = list(
             family = "Open Sans, sans-serif",
             size = 12
           ),
           type = "log",
           range = c(0, 5)),
         yaxis = list(
           title = "Number of IP addresses",
           titlefont = list(
             family = "Open Sans, sans-serif",
             size = 12
           )
         )) %>%
  plotly_POST(filename = "wikipedia-portal-pageviews-histogram", sharing = "public")

thresholds <- c(10, 100, 250, 500, 750, 1e3, 2e3, 5e3, 1e4, 1.5e4, 2e4, 4e4, 5e4)
dplyr::data_frame(threshold = polloi::compress(thresholds, 2),
                  `% of IPs with pageviews <= threshold` = vapply(thresholds, function(threshold) {
                    return(sprintf("%.5f%%", 100 * sum(webrequest_counts$pageviews <= threshold)/length(webrequest_counts$pageviews)))
                  }, ""),
                  `low-volume client PVs` = vapply(thresholds, function(threshold) {
                    return(polloi::compress(sum(webrequest_counts$pageviews[webrequest_counts$pageviews <= threshold]), 3))
                  }, ""),
                  `proportion of total PVs accounted for by low-volume clients` = vapply(thresholds, function(threshold) {
                    return(sprintf("%.2f%%", 100 * sum(webrequest_counts$pageviews[webrequest_counts$pageviews <= threshold]) / sum(webrequest_counts$pageviews)))
                  }, ""),
                  `high-volume client PVs` = vapply(thresholds, function(threshold) {
                    return(polloi::compress(sum(webrequest_counts$pageviews[webrequest_counts$pageviews > threshold]), 3))
                  }, ""),
                  `proportion of total PVs accounted for by high-volume clients` = vapply(thresholds, function(threshold) {
                    return(sprintf("%.2f%%", 100 * sum(webrequest_counts$pageviews[webrequest_counts$pageviews > threshold]) / sum(webrequest_counts$pageviews)))
                  }, "")) %>% knitr::kable("markdown", align = c("l", "r", "r", "r", "r", "r"))
