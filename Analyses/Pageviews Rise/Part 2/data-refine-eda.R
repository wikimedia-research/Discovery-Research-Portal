## Remotely:
webrequests <- data.table::as.data.table(do.call(rbind,
lapply(c(as.Date("2016-06-17"), as.Date("2016-06-18"), as.Date("2016-08-17")),
function(date) {
  cat("Fetching Portal pageviews from ", as.character(date), "\n")
  query <- paste("USE wmf;
                 SELECT
                   ts, client_ip, is_zero,
                   referer, referer_class,
                   geocoded_data['country'] AS country,
                   geocoded_data['city'] AS city,
                   user_agent_map['device_family'] AS device,
                   user_agent_map['browser_family'] AS browser,
                   user_agent_map['browser_major'] AS browser_version,
                   user_agent_map['os_family'] AS os
                 FROM webrequest",
                 wmf::date_clause(date)$date_clause, 
                 " AND uri_host RLIKE('^(www\\.)?wikipedia.org/*$')
                   AND INSTR(uri_path, 'search-redirect.php') = 0
                   AND content_type RLIKE('^text/html')
                   AND webrequest_source = 'text'
                   AND NOT (referer RLIKE('^http://localhost'))
                   AND agent_type = 'user'
                   AND referer_class != 'unknown'
                   AND http_status IN('200', '304');")
  results <- wmf::query_hive(query)
  results <- results[results$referer_class != "", ]
  # Refine
  results$is_zero <- results$is_zero == "true"
  results$date <- date
  results$ts <- lubridate::ymd_hms(results$ts)
  results$unknown_country <- results$country == "Unknown"
  results$usa <- results$country == "United States"
  return(results[, union("date", names(results))])
}))) # 36527327 rows

readr::write_rds(webrequests,
                 paste0("~/portal_webrequests_17June-and-17August.rds"),
                 "gz")

# Continue
webrequests <- readr::read_rds("~/portal_webrequests_17June-and-17August.rds")

# Explore
library(magrittr)
import::from(dplyr, keep_where = filter, group_by, summarize, mutate, arrange, select, ungroup, tally, top_n)

# Are proportions of USA vs not-USA different?
webrequests[, list(pageviews = .N), by = c("date", "usa")] %>%
  group_by(date) %>%
  mutate(prop = pageviews/sum(pageviews))
# Nope.

counts_by_ip <- webrequests[webrequests$date >= "2016-06-18",
                            list(pageviews = .N),
                            by = c("date", "client_ip")]

readr::write_tsv(counts_by_ip, "~/portal_webrequest_counts_by_ip-all.tsv")

top_ips <- counts_by_ip %>%
  group_by(date) %>%
  top_n(10, pageviews) %>%
  { .$client_ip }
webrequests$top10 <- webrequests$client_ip %in% top_ips

webrequests[webrequests$date == "2016-08-17",
            list(pageviews = .N),
            by = "top10"] %>%
  mutate(prop = pageviews/sum(pageviews))

webrequests[webrequests$date >= "2016-06-18" & webrequests$top10,
            list(pageviews = .N),
            by = c("date", "client_ip", "country", "city", "device", "os", "browser")] %>%
  arrange(client_ip, date, country, city, device, os, browser) %>%
  readr::write_tsv("~/portal_webrequest_counts_by_ip-top10.tsv")
