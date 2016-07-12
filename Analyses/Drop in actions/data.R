start_date <- as.Date("2016-05-11")
end_date <- as.Date("2016-07-11")
events <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching Portal EL data from", as.character(date), "\n")
  data <- wmf::build_query("SELECT
                              LEFT(timestamp, 8) AS date,
                              event_session_id AS session_id,
                              timestamp AS ts,
                              userAgent AS user_agent,
                              event_event_type AS type,
                              event_section_used AS section_used,
                              LEFT(UPPER(event_country), 2) AS country,
                              CASE WHEN LEFT(event_country, 2) = 'US' THEN UPPER(SUBSTRING(event_country, 4, 5))
                                   ELSE NULL END AS us_state",
                           date = date,
                           table = "WikipediaPortal_14377354",
                           conditionals = "((event_cohort IS NULL) OR (event_cohort IN ('null','baseline')))")
  return(data)
}))
library(magrittr)
events$date %<>% lubridate::ymd()
events$ts %<>% lubridate::ymd_hms()
events$identity <- vapply(paste(events$session_id, events$user_agent, events$country, sep = "|"), httr::sha1_hash, "VALUE", key = "Portal Event Logging")
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
# sum(events$country == "US" & is.na(events$us_abbrv), na.rm = TRUE) # should be 0 after the patch

uniques <- dplyr::distinct(events[, c("user_agent", "identity")])
ua_data <- uaparser::parse_agents(uniques$user_agent, fields = c("device", "os", "os_major", "browser", "browser_major"))
ua_data$identity <- uniques$identity
events <- dplyr::left_join(events, ua_data, by = "identity")

events <- events %>%
  dplyr::group_by(identity) %>%
  dplyr::arrange(ts) %>%
  dplyr::mutate(visit = cumsum(type == "landing")) %>%
  dplyr::ungroup() %>%
  dplyr::filter(visit > 0)

## To investigate later:
# > length(unique(events$session_id))
# [1] 14610
# > length(unique(events$identity))
# [1] 14665

events <- dplyr::select(events, date, ts, identity, session_id, visit, type, section_used, country_name, us_state, device, os, os_major, browser, browser_major)

readr::write_rds(events, "~/portal-T139109-data.rds", "gz")

dir.create("data")
system("scp stat2:/home/bearloga/portal-T139109-data.rds data/")
