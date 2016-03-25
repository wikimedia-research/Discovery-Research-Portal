library(magrittr)
import::from(dplyr, group_by, summarize, mutate, select, rename, arrange, keep_where = filter, ungroup, top_n)
library(ggplot2)
correct_proportion <- function(x) {
  vapply(x, function(x, y) { return(min(x, y)) }, 0.0, y = 1)
}
choropleth_theme <- function() {
  return(ggthemes::theme_fivethirtyeight() +
           theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
                 legend.key.width = unit(0.6, "in"), legend.background = element_blank(),
                 panel.background = element_blank(), plot.background = element_blank(),
                 panel.grid = element_line(colour = "gray20", linetype = "dashed")))
}

aggregates <- readr::read_csv('portal-js.csv')
requests <- keep_where(aggregates, date > as.Date("2016-02-05"))

dir.create('figs')

p <- aggregates %>%
  group_by(date) %>%
  summarize(index_js = sum(index_js), wikipedia_wordmark = sum(wikipedia_wordmark)) %>%
  ggplot(data = ., aes(x = date, y = wikipedia_wordmark)) +
  geom_line(color = "black", size = 1.1) +
  geom_point(color = "black", size = 3) +
  geom_line(aes(y = index_js), color = "cornflowerblue", size = 1.1, alpha = 0.75) +
  geom_point(aes(y = index_js), color = "cornflowerblue", size = 3, alpha = 0.75) +
  geom_text(aes(y = index_js + 6e4, label = sprintf("%.2f%%", 100*index_js/wikipedia_wordmark))) +
  scale_y_continuous(name = "Requests", breaks = seq(5e5, 3e6, 5e5), labels = polloi::compress, limits = c(0, 3e6)) +
  ggtitle("Daily JS support from sampled requests on Wikipedia Portal") +
  annotate("text", x = as.Date("2016-02-05"), y = 5e5, label = "JS patch deployed") +
  ggthemes::theme_gdocs() +
  theme(rect = element_blank()) +
  xlab("Date")
ggsave("figs/daily-js.png", p, width = 10, height = 5, units = "in", dpi = 150)

requests %>%
  group_by(date, country) %>%
  summarize(index_js = sum(index_js), wikipedia_wordmark = sum(wikipedia_wordmark)) %>%
  mutate(proportion = correct_proportion(index_js / wikipedia_wordmark)) %>%
  group_by(country) %>%
  summarize(proportion = median(proportion)) %>%
  summarize(`Min %` = min(proportion),
            `25th Percentile` = quantile(proportion, 0.25),
            `Average %` = mean(proportion),
            `Median %` = median(proportion),
            `75th Percentile` = quantile(proportion, 0.75),
            `99th Percentile` = quantile(proportion, 0.99)) %>%
            { .[1, ] <- sprintf("%.2f%%", 100 * .[1, ]); . } %>%
  knitr::kable(align = "r", caption = "Summary of JS support % across 235 countries.",
               format = "html")

worldwide_js_support <- requests %>%
  group_by(date, country) %>%
  summarize(index_js = sum(index_js), wikipedia_wordmark = sum(wikipedia_wordmark)) %>%
  keep_where(wikipedia_wordmark > 1e2) %>%
  mutate(proportion = correct_proportion(index_js / wikipedia_wordmark)) %>%
  group_by(country) %>%
  summarize(proportion = median(proportion))

p <- ggplot(data = worldwide_js_support) +
  geom_map(data = world_map, aes(map_id = region), fill = "white", map = world_map, color = "black", size = 0.1) +
  geom_map(aes(map_id = country, fill = proportion), map = world_map, color = "black", size = 0.25) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_gradientn(name = "% of requests with JS support: ",
                       limits = c(0.5, 1),
                       colors = RColorBrewer::brewer.pal(5, "YlGnBu"),
                       labels = scales::percent_format()) +
  ggtitle("JS support (from countries with >100 WP Wordmark requests)") +
  choropleth_theme()
ggsave("figs/global-js-100.png", p, width = 10, height = 7, units = "in", dpi = 300)

worldwide_js_support <- requests %>%
  group_by(date, country) %>%
  summarize(index_js = sum(index_js), wikipedia_wordmark = sum(wikipedia_wordmark)) %>%
  mutate(proportion = correct_proportion(index_js / wikipedia_wordmark)) %>%
  group_by(country) %>%
  summarize(proportion = median(proportion))

p <- ggplot(data = worldwide_js_support) +
  geom_map(data = world_map, aes(map_id = region), fill = "white", map = world_map, color = "black", size = 0.1) +
  geom_map(aes(map_id = country, fill = proportion), map = world_map, color = "black", size = 0.25) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_gradientn(name = "% of requests with JS support: ",
                       limits = c(0.5, 1),
                       colors = RColorBrewer::brewer.pal(5, "YlGnBu"),
                       labels = scales::percent_format()) +
  ggtitle("JS support") +
  choropleth_theme()
ggsave("figs/global-js.png", p, width = 10, height = 7, units = "in", dpi = 300)

p <- requests %>%
  group_by(date, country) %>%
  summarize(wikipedia_wordmark = sum(wikipedia_wordmark)) %>%
  group_by(country) %>%
  summarize(avg = median(wikipedia_wordmark)) %>%
  ggplot(data = .) +
  geom_map(data = world_map, aes(map_id = region), fill = "white", map = world_map, color = "black", size = 0.05) +
  geom_map(aes(map_id = country, fill = avg), map = world_map, color = "black", size = 0.1) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_gradientn(name = "Sampled requests",
                       colors = RColorBrewer::brewer.pal(5, "YlGnBu"),
                       labels = polloi::compress) +
  ggtitle("Sources of sampled requests (median of 5 days)") +
  choropleth_theme()
ggsave("figs/global-traffic.png", p, width = 10, height = 7, units = "in", dpi = 300)

p <- requests %>%
  group_by(date, country) %>%
  summarize(wikipedia_wordmark = sum(wikipedia_wordmark)) %>%
  group_by(country) %>%
  summarize(avg = median(wikipedia_wordmark)) %>%
  ggplot(data = .) +
  geom_map(data = world_map, aes(map_id = region), fill = "white", map = world_map, color = "black", size = 0.05) +
  geom_map(aes(map_id = country, fill = avg), map = world_map, color = "black", size = 0.1) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_gradientn(name = "Sampled requests",
                       limits = c(1e0, 2.2e5),
                       colors = RColorBrewer::brewer.pal(5, "YlGnBu"),
                       labels = polloi::compress) +
  ggtitle("Sources of sampled requests (median of 5 days, excluding U.S.)") +
  choropleth_theme()
ggsave("figs/global-traffic-sansUS.png", p, width = 10, height = 7, units = "in", dpi = 300)

requests %>%
  group_by(date, country) %>%
  summarize(wikipedia_wordmark = sum(wikipedia_wordmark), index_js = sum(index_js)) %>%
  group_by(country) %>%
  summarize(`Wordmark Rqsts` = median(wikipedia_wordmark),
            `JS Rqsts` = median(index_js)) %>%
  mutate(`% of Total` = sprintf("%.2f%%", 100 * correct_proportion(`Wordmark Rqsts` / sum(`Wordmark Rqsts`))),
         `Approx. % with JS support (JS/Wordmark)` = sprintf("%.2f%%", 100 * correct_proportion(`JS Rqsts` / `Wordmark Rqsts`))) %>%
  top_n(20, `% of Total`) %>%
  arrange(desc(`Wordmark Rqsts`)) %>%
  mutate(`Wordmark Rqsts` = polloi::compress(`Wordmark Rqsts`),
         `JS Rqsts` = polloi::compress(`JS Rqsts`)) %>%
  knitr::kable(caption = "Top 20 countries by % of traffic and the corresponding approximate percentage of requests ('users') with JS support.", align = "r",
               format = "html")

top_browsers <- requests %>%
  group_by(date, browser) %>%
  summarize(wikipedia_wordmark = sum(wikipedia_wordmark)) %>%
  group_by(browser) %>%
  summarize(wikipedia_wordmark = median(wikipedia_wordmark)) %>%
  mutate(share = wikipedia_wordmark/sum(wikipedia_wordmark)) %>%
  keep_where(share > 0.01) %>%
  arrange(desc(share))

browser_js <- requests %>%
  keep_where(browser %in% top_browsers$browser) %>%
  group_by(date, browser) %>%
  summarize(index_js = sum(index_js), wikipedia_wordmark = sum(wikipedia_wordmark)) %>%
  mutate(proportion = index_js / wikipedia_wordmark) %>%
  group_by(browser) %>%
  summarize(`median JS support` = median(proportion)) %>%
  dplyr::left_join(top_browsers, by = "browser")

p <- browser_js %>%
  mutate(`median JS support` = correct_proportion(`median JS support`)) %>%
  ggplot(data = ., aes(y = `median JS support`, x = browser)) +
  geom_pointrange(aes(ymax = `median JS support`, ymin = 0)) +
  geom_point(color = "black", size = 4) +
  geom_point(aes(color = share), size = 3) +
  scale_x_discrete(limits = browser_js$browser[order(browser_js$share, decreasing = FALSE)]) +
  scale_y_continuous(breaks = seq(0, 1.25, 0.10), labels = scales::percent_format()) +
  coord_flip() +
  ggtitle("Proportion of JS support in requests from 'top browsers'") +
  xlab("Browser") +
  ylab("% of Requests With JS Support") +
  scale_color_gradientn(name = "% of Total Traffic",
                        limits = c(0, 0.2),
                        colors = RColorBrewer::brewer.pal(5, "YlGnBu"),
                        labels = scales::percent_format()) +
  ggthemes::theme_gdocs() +
  theme(rect = element_blank())
ggsave("figs/browsers.png", p, width = 10, height = 7, units = "in", dpi = 300)
