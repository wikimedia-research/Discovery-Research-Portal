## Remotely:
end_date <- Sys.Date() - 1
start_date <- end_date - 61
webrequests <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
cat("Fetching Portal pageviews from ", as.character(date), "\n")
  query <- paste("USE wmf;
                SELECT
                  http_status, is_pageview, agent_type, referer_class, COUNT(1) AS web_requests
                FROM webrequest",
               wmf::date_clause(date)$date_clause, 
               " AND uri_host RLIKE('^(www\\.)?wikipedia.org/*$')
                 AND INSTR(uri_path, 'search-redirect.php') = 0
                 AND content_type RLIKE('^text/html')
                 AND webrequest_source = 'text'
                 AND NOT (referer RLIKE('^http://localhost'))
               GROUP BY http_status, is_pageview, agent_type, referer_class;")
  results <- wmf::query_hive(query)
  results <- results[results$referer_class != "", ]
  results$date <- date
  return(results[, union("date", names(results))])
}))

readr::write_rds(data.table::as.data.table(webrequests),
                 paste0("~/portal_webrequest_counts_", as.character(start_date, "%Y%m%d"), "-", as.character(end_date, "%Y%m%d"), ".rds"),
                 "gz")

q(save = "no")

## Locally:
dir.create("data")
system2("scp", c("stat2:/home/bearloga/portal_webrequest_counts_20160617-20160817.rds", "data/"))

## Download dashboard-version pageviews:
pageview_data <- data.table::as.data.table(polloi::read_dataset(path = "portal/portal_pageviews.tsv"))
readr::write_rds(pageview_data, "data/portal_pageviews.rds", "gz")
