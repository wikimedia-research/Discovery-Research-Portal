## Remotely:

# Group by country, UA, referer, http_status
end_date <- Sys.Date() - 1
start_date <- end_date - 61
webrequests <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
cat("Fetching Portal pageviews from ", as.character(date), "\n")
  query <- paste("USE wmf;
                SELECT
                  geocoded_data['country'] AS country,
                  user_agent_map['os_family'] AS os,
                  user_agent_map['browser_family'] AS browser,
                  user_agent_map['device_family'] AS device,
                  access_method,
                  referer_class,
                  http_status, 
                  COUNT(1) AS web_requests
                FROM webrequest",
               wmf::date_clause(date)$date_clause, 
               " AND uri_host RLIKE('^(www\\.)?wikipedia.org/*$')
                 AND INSTR(uri_path, 'search-redirect.php') = 0
                 AND content_type RLIKE('^text/html')
                 AND webrequest_source = 'text'
                 AND NOT (referer RLIKE('^http://localhost'))
                 AND agent_type = 'user'
                 AND referer_class != 'unknown'
                 AND http_status IN('200', '304')
               GROUP BY geocoded_data['country'], user_agent_map['os_family'], user_agent_map['browser_family'], user_agent_map['device_family'], access_method, referer_class, http_status;")
  results <- wmf::query_hive(query)
  #results <- results[results$referer_class != "", ]
  results$date <- date
  return(results[, union("date", names(results))])
}))

readr::write_rds(data.table::as.data.table(webrequests),
                 paste0("~/portal_webrequest_counts_", as.character(start_date, "%Y%m%d"), "-", as.character(end_date, "%Y%m%d"), ".rds"),
                 "gz")

q(save = "no")

# Group by several percentile buckets
end_date <- Sys.Date() - 1
start_date <- end_date - 61
pageviews <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching Portal pageviews from ", as.character(date), "\n")
  query <- paste("USE wmf;
                SELECT
                  client_ip,
                  COUNT(1) AS pageviews
                FROM webrequest",
                wmf::date_clause(date)$date_clause, 
               " AND uri_host RLIKE('^(www\\.)?wikipedia.org/*$')
                 AND INSTR(uri_path, 'search-redirect.php') = 0
                 AND content_type RLIKE('^text/html')
                 AND webrequest_source = 'text'
                 AND NOT (referer RLIKE('^http://localhost'))
                 AND agent_type = 'user'
                 AND referer_class != 'unknown'
                 AND http_status IN('200', '304')
                GROUP BY client_ip;")
  results <- wmf::query_hive(query)
  results <- data.table::as.data.table(results[!is.na(results$pageviews), ])
  perc_seq <- c(quantile(results$pageviews, c(1, 0.9999, 0.99, 0.95, 0.9, 0.8)), min(results$pageviews)-1)
  results$perc_buckets <- cut(results$pageviews, perc_seq, right=T, labels=letters[1:(length(perc_seq)-1)]) # a to f increased quantile buckets
  
  output <- cbind(date = date,
                  tidyr::spread(results[, list(pageviews = sum(pageviews)), by = "perc_buckets"],
                                perc_buckets, pageviews),
                  pageviews = sum(results$pageviews))
 
  return(output)
}))

readr::write_rds(data.table::as.data.table(pageviews),
                 paste0("~/portal_pageviews_counts_", as.character(start_date, "%Y%m%d"), "-", as.character(end_date, "%Y%m%d"), ".rds"),
                 "gz")

q(save = "no")

# Group by several percentile buckets, US only
end_date <- Sys.Date() - 1
start_date <- end_date - 31
pageviews <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching Portal pageviews from ", as.character(date), "\n")
  query <- paste("USE wmf;
                 SELECT
                 client_ip,
                 COUNT(1) AS pageviews
                 FROM webrequest",
                 wmf::date_clause(date)$date_clause, 
                 " AND uri_host RLIKE('^(www\\.)?wikipedia.org/*$')
                 AND INSTR(uri_path, 'search-redirect.php') = 0
                 AND content_type RLIKE('^text/html')
                 AND webrequest_source = 'text'
                 AND NOT (referer RLIKE('^http://localhost'))
                 AND agent_type = 'user'
                 AND referer_class != 'unknown'
                 AND http_status IN('200', '304')
                 AND geocoded_data['country'] = 'United States'
                 GROUP BY client_ip;")
  results <- wmf::query_hive(query)
  results <- data.table::as.data.table(results[!is.na(results$pageviews), ])
  perc_seq <- c(quantile(results$pageviews, c(1, 0.9999, 0.99, 0.95, 0.9)), min(results$pageviews)-1)
  results$perc_buckets <- cut(results$pageviews, perc_seq, right=T, labels=letters[1:(length(perc_seq)-1)]) # a to m increase
  
  output <- cbind(date = date,
                  tidyr::spread(results[, list(pageviews = sum(pageviews)), by = "perc_buckets"],
                                perc_buckets, pageviews),
                  pageviews = sum(results$pageviews))
  
  return(output)
}))

readr::write_rds(data.table::as.data.table(pageviews),
                 paste0("~/portal_us_pageviews_counts_", as.character(start_date, "%Y%m%d"), "-", as.character(end_date, "%Y%m%d"), ".rds"),
                 "gz")

q(save = "no")

# Top 30 IP everyday
end_date <- Sys.Date() - 1
start_date <- end_date - 61
pageviews <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching Portal pageviews from ", as.character(date), "\n")
  query <- paste("USE wmf;
                 SELECT
                 client_ip,
                 geocoded_data['country'] as country,
                 geocoded_data['city'] as city,
                 geocoded_data['postal_code'] as postal_code,
                 COUNT(1) AS pageviews
                 FROM webrequest",
                 wmf::date_clause(date)$date_clause, 
                 " AND uri_host RLIKE('^(www\\.)?wikipedia.org/*$')
                 AND INSTR(uri_path, 'search-redirect.php') = 0
                 AND content_type RLIKE('^text/html')
                 AND webrequest_source = 'text'
                 AND NOT (referer RLIKE('^http://localhost'))
                 AND agent_type = 'user'
                 AND referer_class != 'unknown'
                 AND http_status IN('200', '304')
                 GROUP BY client_ip, geocoded_data['country'], geocoded_data['city'], geocoded_data['postal_code']
                 ORDER BY pageviews DESC
                 LIMIT 30;")
  results <- wmf::query_hive(query)
  results$date <- date
  results <- data.table::as.data.table(results[!is.na(results$pageviews), ])
  return(results)
}))

readr::write_rds(data.table::as.data.table(pageviews),
                 paste0("~/portal_pageviews_top30IP_counts_", as.character(start_date, "%Y%m%d"), "-", as.character(end_date, "%Y%m%d"), ".rds"),
                 "gz")

q(save = "no")


# Group by os, browser, check android
query <- "SELECT
user_agent_map['os_family'] AS os,
user_agent_map['os_major'] AS os_major,
user_agent_map['os_minor'] AS os_minor,
user_agent_map['browser_family'] AS browser,
user_agent_map['browser_major'] AS browser_major,
COUNT(1) AS web_requests
FROM webrequest
WHERE year=2016 AND month=10 AND day=4
AND uri_host RLIKE('^(www\\.)?wikipedia.org/*$')
AND INSTR(uri_path, 'search-redirect.php') = 0
AND content_type RLIKE('^text/html')
AND webrequest_source = 'text'
AND NOT (referer RLIKE('^http://localhost'))
AND agent_type = 'user'
AND referer_class != 'unknown'
AND http_status IN('200', '304')
GROUP BY user_agent_map['os_family'],user_agent_map['os_major'],user_agent_map['os_minor'],user_agent_map['browser_family'],user_agent_map['browser_major'];
"

## Locally:
dir.create("data")
system2("scp", c("stat2:/home/chelsyx/portal_webrequest_counts_20160804-20161004.rds", "data/"))
system2("scp", c("stat2:/home/chelsyx/portal_pageviews_counts_20160804-20161004.rds", "data/"))
system2("scp", c("stat2:/home/chelsyx/portal_us_pageviews_counts_20160904-20161005.rds", "data/"))
system2("scp", c("stat2:/home/chelsyx/portal_pageviews_top30IP_counts_20160806-20161006.rds", "data/"))

## Download dashboard-version pageviews:
pageview_data <- data.table::as.data.table(polloi::read_dataset(path = "portal/portal_pageviews.tsv"))
readr::write_rds(pageview_data, "data/portal_pageviews.rds", "gz")
