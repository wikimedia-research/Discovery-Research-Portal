query <- paste("ADD JAR /home/bearloga/Code/analytics-refinery-jars/refinery-hive.jar;
               CREATE TEMPORARY FUNCTION classify_referrer AS
               'org.wikimedia.analytics.refinery.hive.SmartReferrerClassifierUDF';
               USE wmf;
               SELECT referrer, referrer_class, COUNT(1) AS requests
               FROM (
                 SELECT
                   referer AS referrer,
                   classify_referrer(referer) AS referrer_class
                 FROM webrequest ", wmf::date_clause(Sys.Date()-1)$date_clause, "
                   AND webrequest_source = 'text'
                   AND content_type RLIKE('^text/html')
                   AND uri_host IN('www.wikipedia.org','wikipedia.org')
                   AND classify_referrer(referer) IN ('internal', 'external', 'unknown')
                   AND NOT (referer RLIKE('^(https?://(www\\.)?)?wikipedia\\.org.*$'))
                   AND uri_path = '/search-redirect.php'
                   AND NOT (referer RLIKE('^http://localhost'))
               ) AS refined_webrequests
               GROUP BY referrer, referrer_class;")
results <- wmf::query_hive(query)
results <- results[!is.na(results$requests), ]
results <- results[order(results$requests, decreasing = TRUE), ]
head(results, 10)
# |    |referrer                              |referrer_class | requests|
# |:---|:-------------------------------------|:--------------|--------:|
# |50  |http://www.wikipedia.nl/              |external       |     4926|
# |210 |http://wikipedia.nl/                  |external       |     1893|
# |199 |https://www.wikivoyage.org/           |internal       |      927|
# |298 |http://portal.wikimedia.ch/wikipedia  |external       |      905|
# |70  |http://www.wikipedia.be/              |external       |      316|
# |73  |http://wikimedia.in/wikipedia.html    |external       |      224|
# |423 |http://portal.wikimedia.ch/wikipedia/ |external       |      155|
# |114 |https://duckduckgo.com                |external       |       75|
# |274 |http://www.wikinews.org/              |internal       |       67|
# |228 |http://wikipedia.be/                  |external       |       64|

end_date <- Sys.Date() - 1
start_date <- end_date
referrals <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching Portal referers from ", as.character(date), "\n")
  query <- paste("ADD JAR /home/bearloga/Code/analytics-refinery-jars/refinery-hive.jar;
                 CREATE TEMPORARY FUNCTION classify_referrer AS 'org.wikimedia.analytics.refinery.hive.SmartReferrerClassifierUDF';
                 USE wmf;
                 SELECT request, referrer, COUNT(1) AS requests
                 FROM (
                   SELECT
                     CASE WHEN referer RLIKE('^(https?://www\\.)?wikipedia\\.org/+search-redirect\\.php\\??.*') THEN 'search-redirect.php'
                          WHEN referer RLIKE('^(https?://(www\\.)?)?wikipedia\\.org.*$') THEN 'Wikipedia Portal'
                          ELSE 'other' END AS referrer,
                     CASE WHEN uri_path = '/search-redirect.php' THEN 'search-redirect.php'
                          ELSE 'other' END AS request
                   FROM webrequest ", wmf::date_clause(date)$date_clause, "
                     AND webrequest_source = 'text'
                     AND content_type RLIKE('^text/html')
                     AND uri_host RLIKE('^(www\\.)?wikipedia.org/*$')
                     AND classify_referrer(referer) IN ('internal', 'external', 'unknown')
                     AND NOT (referer RLIKE('^http://localhost'))
                 ) AS refined_webrequests
                 GROUP BY request, referrer;")
  results <- wmf::query_hive(query)
  results <- results[!is.na(results$requests), ]
  results <- results[order(results$request, results$referrer), ]
  return(cbind(date = date, results, stringsAsFactors = FALSE))
}))

referrals$proportion <- referrals$requests/sum(referrals$requests)
rownames(referrals) <- NULL
knitr::kable(referrals, digits = 3)
