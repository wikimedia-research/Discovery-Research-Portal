source("config.R")
library(data.table)
library(magrittr)
library(ggplot2)

# pageviews <- readr::read_tsv("data/pageview_counts_portal-ukwiki_20160805-20160825.tsv", col_types = "Dccccclllil")
# pageviews <- readr::read_tsv("data/pageview_counts_portal-ukwiki_20160629-20160828.tsv", col_types = "Dccccclllil")
pageviews <- readr::read_tsv("data/pageview_counts_portal-ukwiki_20160703-20160901.tsv", col_types = "Dcclccclllil")
pageviews <- dplyr::left_join(pageviews, polloi::get_prefixes()[, c("language", "prefix")], by = "prefix")
pageviews <- as.data.table(pageviews)

pageviews$from <- ifelse(pageviews$from_wikipedia_portal, "from Wikipedia.org Portal", "from elsewhere")
pageviews$from[pageviews$from_search_engine] <- "from a search engine"
pageviews$from[pageviews$referer_class == "none"] <- "direct (e.g. bookmark or homepage)"
pageviews$from[pageviews$referer_class == "internal" & !pageviews$from_wikipedia_portal] <- "from a Wikimedia project/tool/bot"

temp <- pageviews[, list(pageviews = sum(pageviews)), by = c("date", "project", "language", "from")] %>%
  dplyr::filter(project == "wikipedia", language != "English")

{
  ggplot(temp, aes(x = date, y = pageviews, color = language)) +
  geom_line(size = 1.01) +
  scale_color_brewer("Wikipedia", palette = "Set1",
                     limits = c("Russian", "Ukrainian", "Crimean Tatar", "German")) +
  # scale_y_continuous(labels = polloi::compress) +
  scale_x_date("Date", date_breaks = "2 week", date_labels = "%d %b") +
  scale_y_log10("Pageviews", labels = polloi::compress, breaks = c(1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7)) +
  facet_wrap( ~ from, nrow = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2016-08-16")), linetype = "dashed") +
  ggthemes::theme_base(base_size = 12, base_family = "Helvetica Neue") +
  theme(legend.position = "bottom", rect = element_rect(color = NA),
        panel.border = element_rect(colour = "black")) +
  ggtitle("Pageviews to Russian, Ukrainian, Crimean Tatar, and German Wikipedias",
          subtitle = "Dashed line represents August 16th deployment of the secondary link collapse on Wikipedia.org Portal")
  } %>% ggsave("pageviews_no-enwiki.png", plot = ., path = figures_dir,
               width = 15, height = 5, dpi = 300)
