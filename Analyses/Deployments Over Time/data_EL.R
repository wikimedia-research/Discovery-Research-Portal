start_date <- as.Date("2015-11-11")
end_date <- as.Date("2016-07-20")
events <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching Portal EL data from", as.character(date), "\n")
  data <- tryCatch(wmf::build_query("SELECT
                           LEFT(timestamp, 8) AS date,
                           event_session_id AS session_id,
                           timestamp AS ts,
                           userAgent AS user_agent,
                           event_event_type AS type,
                           event_section_used AS section_used,
                           event_destination AS destination,
                           LEFT(UPPER(event_country), 2) AS country,
                           CASE WHEN LEFT(event_country, 2) = 'US' THEN UPPER(SUBSTRING(event_country, 4, 5))
                           ELSE NULL END AS us_state",
                           date = date,
                           table = "WikipediaPortal_14377354",
                           conditionals = "((event_cohort IS NULL) OR (event_cohort IN ('null','baseline')))"),
                   error = function(e) {
                     cat("No EL data on that day.\n")
                     return(NULL)
                   }, finally = invisible(NULL))
  return(data)
}))
library(magrittr)
events$date %<>% lubridate::ymd()
events$ts %<>% lubridate::ymd_hms()

## Refinement:

unique_ua <- unique(events$user_agent)
ua_data <- uaparser::parse_agents(unique_ua, fields = c("device", "os", "os_major", "browser", "browser_major"))
ua_data$user_agent <- unique_ua
events <- dplyr::left_join(events, ua_data, by = "user_agent")

# events$identity <- vapply(paste(events$session_id, events$user_agent, events$country, sep = "|"), httr::sha1_hash, "VALUE", key = "Portal Event Logging")
events$identity <- sprintf("%010.0f", as.numeric(factor(paste(events$session_id, events$user_agent, events$country, sep = "|"))))

events <- events[order(events$identity, events$ts, events$type), ]

data("ISO_3166_1", package = "ISOcodes")
ISO_3166_1 <- dplyr::bind_rows(
  ISO_3166_1,
  data.frame(Alpha_2 = c("A1", "AP", "EU", "A2"),
             Name = c("Anonymous Proxy", "Asia/Pacific Region", "Europe", "Satellite Provider"),
             stringsAsFactors = FALSE)
)

events <- dplyr::left_join(events, ISO_3166_1[, c("Alpha_2", "Name")], by = c("country" = "Alpha_2"))
events <- dplyr::rename(events, country_name = Name)
# table(events$country[is.na(events$country_name)]) # should be empty!

states <- data.frame(abbrv = state.abb, name = state.name, stringsAsFactors = FALSE)
events <- dplyr::left_join(events, states, by = c("us_state" = "abbrv"))
events <- dplyr::rename(events, us_abbrv = us_state, us_state = name)
events$us_abbrv[events$us_abbrv == ""] <- as.character(NA)

events$destination[grepl("wikipedia.org/search-redirect.php", events$destination, fixed = TRUE)] <- "search-redirect.php"
events$destination <- sub("https://", "", events$destination, fixed = TRUE)
events$destination <- sub("http://", "", events$destination, fixed = TRUE)
events$destination <- sub("(.*.\\org/)wiki/.*", "\\1", events$destination)
events$destination <- sub("wikipedia.org/", "wikipedia.org", events$destination, fixed = TRUE)

events <- events %>%
  dplyr::group_by(identity) %>%
  dplyr::arrange(ts) %>%
  dplyr::mutate(visit = cumsum(type == "landing")) %>%
  dplyr::ungroup() %>%
  dplyr::filter(visit > 0)

# For space concerns, let's only keep (1) visits 1-10 for each session and (2) a maximum of 2 hours of events per session
events <- dplyr::filter(events, visit < 11)
session_lengths <- data.table::as.data.table(events)[, list(session_length = as.numeric(difftime(max(ts), min(ts), units = "secs"))), by = "identity"]
identities <- session_lengths$identity[session_lengths$session_length <= 7200]
events <- dplyr::filter(events, identity %in% identities)

events <- dplyr::select(events, identity, session_id, date, ts, visit, type, section_used, destination, country, country_name, us_state, device, os, os_major, browser, browser_major)

readr::write_rds(events, "~/portal-T138397-data.rds", "gz")

dir.create("data")
system("scp stat2:/home/bearloga/portal-T138397-data.rds data/")
