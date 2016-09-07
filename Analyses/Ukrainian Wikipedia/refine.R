source("config.R")

library(data.table)
library(magrittr)

pageviews <- readr::read_tsv("data/pageview_counts_portal-ukwiki_20160703-20160905.tsv", col_types = "Dcclccclllcllil")
pageviews <- dplyr::left_join(pageviews, polloi::get_prefixes()[, c("language", "prefix")], by = "prefix")
pageviews <- as.data.table(pageviews)

pageviews$post_deployment_indicator <- as.numeric(pageviews$post_deployment)
pageviews$site <- paste(tolower(pageviews$language), pageviews$project)
pageviews$page <- ifelse(pageviews$is_main_page, "(main page)", "(other pages)")
pageviews$from <- ifelse(pageviews$from_wikipedia_portal, "from Wikipedia.org Portal", "from elsewhere")
pageviews$from[pageviews$from_search_redirect] <- "from Wikipedia.org Portal (search-redirect.php)"
pageviews$from[pageviews$from_search_engine] <- "from a search engine"
pageviews$from[pageviews$referer_class == "none"] <- "direct (e.g. bookmark or homepage)"
pageviews$from[pageviews$referer_class == "internal" & !pageviews$from_wikipedia_portal] <- "from a Wikimedia project/tool/bot"
pageviews$source <- paste(pageviews$site, pageviews$page, pageviews$from)
pageviews$speaks <- "users who don't speak Russian or Ukrainian"
pageviews$speaks[pageviews$speaks_ukrainian & !pageviews$speaks_russian] <- "users who speak Ukrainian"
pageviews$speaks[!pageviews$speaks_ukrainian & pageviews$speaks_russian] <- "users who speak Russian"
pageviews$speaks[pageviews$speaks_ukrainian & pageviews$speaks_russian] <- "users who speak Ukrainian and Russian"
pageviews$source_speaks <- paste0(pageviews$source, " (from ", pageviews$speaks, ")")

pageviews <- pageviews[order(date, source, speaks, country), ]
