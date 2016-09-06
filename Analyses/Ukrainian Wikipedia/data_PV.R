start_date <- as.Date("2016-09-02") # Sys.Date() - 61
end_date <- Sys.Date() - 1
pageviews <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching pageview counts from", as.character(date, "%d %B %Y"), "\n")
  query <- paste0("ADD JAR /home/bearloga/Code/analytics-refinery-jars/refinery-hive.jar;
                  CREATE TEMPORARY FUNCTION is_external_search AS
                    'org.wikimedia.analytics.refinery.hive.IsExternalSearchUDF';
                  CREATE TEMPORARY FUNCTION classify_referer AS
                    'org.wikimedia.analytics.refinery.hive.SmartReferrerClassifierUDF';
                  CREATE TEMPORARY FUNCTION get_engine AS
                    'org.wikimedia.analytics.refinery.hive.IdentifySearchEngineUDF';
                  USE wmf;
                  SELECT
                    project, prefix, is_main_page,
                    access_method, referer_class,
                    search_engine, from_search_engine,
                    from_wikipedia_portal, from_search_redirect,
                    country, speaks_ukrainian, speaks_russian,
                    COUNT(1) AS pageviews
                  FROM (
                    SELECT
                      normalized_host.project_class as project,
                      normalized_host.project AS prefix,
                      is_external_search(referer) AS from_search_engine,
                      classify_referer(referer) AS referer_class,
                      get_engine(referer) as search_engine,
                      access_method,
                      CASE WHEN geocoded_data['country'] = 'Ukraine' THEN 'Ukraine'
                           WHEN geocoded_data['country'] = 'Unknown' THEN 'Unknown'
                           ELSE 'Other' END AS country,
                      CASE WHEN INSTR(accept_language, 'uk') > 0 THEN 'TRUE' ELSE 'FALSE' END AS speaks_ukrainian,
                      CASE WHEN INSTR(accept_language, 'ru') > 0 THEN 'TRUE' ELSE 'FALSE' END AS speaks_russian,
                      CASE WHEN referer RLIKE('^https?://(www\\.)?wikipedia\\.org/*$')
                           THEN 'TRUE' ELSE 'FALSE' END AS from_wikipedia_portal,
                      CASE WHEN
                           uri_path IN(
                             '/wiki/%D0%93%D0%BE%D0%BB%D0%BE%D0%B2%D0%BD%D0%B0_%D1%81%D1%82%D0%BE%D1%80%D1%96%D0%BD%D0%BA%D0%B0',
                             '/wiki/%D0%97%D0%B0%D0%B3%D0%BB%D0%B0%D0%B2%D0%BD%D0%B0%D1%8F_%D1%81%D1%82%D1%80%D0%B0%D0%BD%D0%B8%D1%86%D0%B0',
                             '/wiki/Ba%C5%9F_Saife',
                             '/wiki/Wikipedia:Hauptseite',
                             '/wiki/Main_Page')
                           THEN 'TRUE' ELSE 'FALSE' END AS is_main_page,
                      CASE WHEN INSTR(referer, 'wikipedia.org/search-redirect.php') > 0
                           THEN 'TRUE' ELSE 'FALSE' END AS from_search_redirect
                    FROM webrequest ",
                    wmf::date_clause(date)$date_clause, "
                      AND webrequest_source = 'text'
                      AND is_pageview = true
                      AND access_method IN('desktop', 'mobile web')
                      AND normalized_host.project IN('uk', 'ru', 'crh', 'de', 'en')
                  ) AS filtered_webrequests
                  GROUP BY
                    project, prefix, is_main_page,
                    access_method, referer_class,
                    search_engine, from_search_engine,
                    from_wikipedia_portal, from_search_redirect,
                    country, speaks_ukrainian, speaks_russian;")
  results <- wmf::query_hive(query, override_jars = TRUE)
  # Hive-queried data requires this filtering step:
  results <- results[!is.na(results$pageviews), ]
  # Post-processing:
  results$from_search_engine <- results$from_search_engine == "true"
  results$date <- date
  results <- results[, union("date", names(results))]
  results$search_engine[results$search_engine == "unknown" | (results$referer_class == "unknown" & results$search_engine == "none")] <- NA
  results$referer_class[results$referer_class == "unknown"] <- NA
  results$referer_class[results$referer_class == "external (search engine)" & (results$search_engine == "none" | is.na(results$search_engine))] <- NA
  results$from_search_engine[results$from_search_engine & results$referer_class == "unknown"] <- NA
  results$from_search_engine[is.na(results$referer_class) | is.na(results$search_engine)] <- NA
  results <- data.table::as.data.table(results)[, list(pageviews = sum(pageviews)),
                                                by = c("date", "project", "prefix", "is_main_page", "access_method",
                                                       "referer_class", "search_engine", "from_search_engine",
                                                       "from_wikipedia_portal", "from_search_redirect",
                                                       "country", "speaks_ukrainian", "speaks_russian")]
  # Finish:
  return(results)
}))

pageviews$post_deployment <- pageviews$date >= "2016-08-16"

# readr::write_tsv(
#   pageviews,
#   paste0("~/pageview_counts_portal-ukwiki_",
#          as.character(start_date, "%Y%m%d"), "-",
#          as.character(end_date, "%Y%m%d"),
#          ".tsv")
# )

old_pageviews <- readr::read_tsv("pageview_counts_portal-ukwiki_20160703-20160901.tsv", col_types = c("Dcclccclllcllil"))
readr::write_tsv(
  rbind(old_pageviews, pageviews),
  paste0("~/pageview_counts_portal-ukwiki_20160703-", as.character(end_date, "%Y%m%d"), ".tsv")
)

q(save = "no")

# Locally
system2("scp", c("-r", "stat2:/home/bearloga/pageview_counts_portal-ukwiki_*.tsv", "data/"))
