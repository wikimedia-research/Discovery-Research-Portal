start_date <- Sys.Date()-8
end_date <- Sys.Date()-1
referrals <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching Portal referers from ", as.character(date), "\n")
  query <- paste("ADD JAR /home/bearloga/Code/refineries/refinery-hive-0.0.28-SNAPSHOT.jar;
                 CREATE TEMPORARY FUNCTION classify_referer AS
                 'org.wikimedia.analytics.refinery.hive.SmartReferrerClassifierUDF';
                 USE wmf;
                 SELECT
                   referer, classify_referer(referer) AS referer_class
                 FROM webrequest", wmf::date_clause(date)$date_clause, "
                   AND webrequest_source = 'text'
                   AND content_type RLIKE('^text/html')
                   AND uri_host IN('www.wikipedia.org','wikipedia.org')
                   AND classify_referer(referer) IN ('internal', 'external', 'unknown');")
  results <- wmf::query_hive(query)
  results <- results[results$referer_class != "", ]
  results <- results[results$referer_class %in% c("internal", "external", "unknown"), ]
  return(cbind(date = date, referer = results, stringsAsFactors = FALSE))
}))

names(referrals) <- c("date", "referer", "referer_class")

parsed <- urltools::url_parse(referrals$referer)

library(magrittr)

parsed$domain %<>% sub("^www\\.", "", .)

useless_requests_sets <- list(`Referred from a file` = parsed$scheme == "file",
                              # `Referred by JS` = grepl("^javascript:", referrals$referer, fixed = FALSE),
                              # `Referred by something, idk` = grepl(",x:window", referrals$referer, fixed = TRUE),
                              `Garbage referer metadata` = grepl('[{"name"', referrals$referer, fixed = TRUE),
                              `Empty scheme` = grepl("^\\s*$", parsed$scheme, fixed = FALSE),
                              `Referred by mailto link` = grepl("mailto:", referrals$referer, fixed = TRUE),
                              `Referred by IPv6` = grepl("http://\\[([0-9a-f]{0,4}:){2,7}([0-9a-f]{1,4})?\\]", referrals$referer, fixed = FALSE),
                              `Referred by hndUnblock.cgi on Cisco/Linksys routers` = grepl("hndUnblock.cgi", referrals$referer, fixed = TRUE),
                              `Referred by ourselves (wikipedia.org)` = parsed$domain == "wikipedia.org",
                              `Referred by Wikimedia Nederland (wikimedia.nl)` = parsed$domain == "wikipedia.nl",
                              `Unknown referrer format` = referrals$referer_class == "unknown",
                              `Internal referer (non-Portal)` = referrals$referer_class == "internal" & !(parsed$domain %in% c("wikipedia.org", "wikipedia.nl")))
useless_requests_set <- Reduce(`|`, useless_requests_sets)

filtered_referrals <- urltools::url_parse(referrals$referer[!useless_requests_set])
filtered_referrals$domain %<>% sub("^www\\.", "", .)
filtered_referrals$date <- referrals$date[!useless_requests_set]
filtered_referrals <- filtered_referrals[, c("date", setdiff(names(filtered_referrals), "date"))]
readr::write_csv(filtered_referrals, "referrals.csv")
system("gzip -f referrals.csv")

## External Traffic
summary_traffic_data <- function() {
  # Read in the initial data
  data <- data.table::as.data.table(polloi::read_dataset(path = "portal/portal_referer_data.tsv"))
  # Format
  data$is_search <- ifelse(data$is_search, "Referred by search", "Not referred by search")
  data$search_engine[data$search_engine %in% c("none","None")] <- "Not referred by search"
  # Write out the overall values for traffic
  interim <- data[, j = list(pageviews = sum(pageviews)),
                  by = c("date", "referer_class")] %>%
    reshape2::dcast(formula = date ~ referer_class, fun.aggregate = sum)
  interim$Total <- interim$None + interim$`Search engine` + interim$Other
  interim$None <- interim$None/interim$Total
  interim$Other <- interim$Other/interim$Total
  interim$`Search engine` <- interim$`Search engine`/interim$Total
  names(interim) <- c("date",
                      "Direct (not referred by anything)",
                      "Referred by something other than search engine",
                      "Referred by a search engine",
                      "Total")
  return(tidyr::gather(interim[, 1:4], "traffic", "percentage", -date))
}()

readr::write_csv(summary_traffic_data(), "traffic_summary.csv")
