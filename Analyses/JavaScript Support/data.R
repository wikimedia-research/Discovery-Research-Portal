query_hive <- function(query){
  
  # Write query out to tempfile and create tempfile for results.
  query_dump <- tempfile()
  cat(query, file = query_dump)
  results_dump <- tempfile()
  
  # Query and read in the results
  system_time <- system.time(
    system(paste0("export HADOOP_HEAPSIZE=1024 && hive -S -f ", query_dump, " > ", results_dump))
  )
  
  try({
    results <- read.delim(results_dump, sep = "\t", quote = "", as.is = TRUE, header = TRUE)
  })
  
  # Clean up and return
  file.remove(query_dump, results_dump)
  return(list(data = results, elapsed = system_time['elapsed']))
}

results <- lapply(4:10, function(day) {
  cat("Fetching data for", day, "Feb 2016.\n")
  query <- paste0("ADD JAR /home/ebernhardson/refinery-hive-0.0.21-SNAPSHOT.jar;
                   CREATE TEMPORARY FUNCTION is_spider as 'org.wikimedia.analytics.refinery.hive.IsSpiderUDF';
                   CREATE TEMPORARY FUNCTION is_wikimedia as 'org.wikimedia.analytics.refinery.hive.IsWikimediaBotUDF';
                   CREATE TEMPORARY FUNCTION ua_parser as 'org.wikimedia.analytics.refinery.hive.UAParserUDF';
                   USE wmf;
                   SELECT date, country_code, country, browser, file, COUNT(1) AS requests FROM (
                     SELECT
                       TO_DATE(ts) AS date,
                       geocoded_data['country_code'] AS country_code,
                       geocoded_data['country'] AS country,
                       CONCAT(ua_parser(user_agent)['browser_family'], ' ', ua_parser(user_agent)['browser_major']) AS browser,
                       CASE
                         WHEN INSTR(uri_path, '/portal/wikipedia.org/assets/js/index-') > 0 THEN 'index_js'
                         ELSE 'wikipedia_wordmark' END
                       AS file
                     FROM webrequest
                     WHERE webrequest_source IN('text', 'mobile')
                      AND year = 2016 AND month = 2 AND day = ", day,"
                      AND uri_host IN('wikipedia.org', 'www.wikipedia.org')
                      AND (
                        INSTR(uri_path, '/portal/wikipedia.org/assets/js/index-') > 0
                        OR
                        INSTR(uri_path, '/portal/wikipedia.org/assets/img/Wikipedia_wordmark') > 0
                      )
                      AND NOT is_spider(user_agent) AND NOT is_wikimedia(user_agent)
                      AND NOT (ua_parser(user_agent)['device_family'] = 'Spider')
                      AND http_status IN('200', '202', '301', '302', '304')
                      AND NOT(geocoded_data['country'] = 'Unknown')
                    ) AS requests
                    GROUP BY date, country_code, country, browser, file;")
  temp <- query_hive(query)
  temp$data <- temp$data[1:(nrow(temp$data) - 2), ]
  return(temp)
})

library(magrittr)

aggregates <- results %>%
  lapply(function(result) { return(result$data) }) %>%
  do.call(rbind, .) %>%
  as.data.frame %>%
  tidyr::spread(file, requests, fill = 0)

# Tidying up...
# aggregates <- aggregates[, c(1:4, 8:9)]
# aggregates <- aggregates[1:(nrow(aggregates) - 3), ]

readr::write_csv(aggregates, "portal-js.csv")
