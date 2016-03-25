start_date <- as.Date("2016-01-17") # as.Date("2016-03-01")
end_date <- Sys.Date()-1
events <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching Portal EL data from ", as.character(date), "\n")
  data <- wmf::build_query("SELECT LEFT(timestamp, 8) AS date,
                            event_session_id AS session,
                            event_destination AS destination,
                            event_event_type AS type,
                            event_section_used AS section_used,
                            timestamp AS ts,
                            userAgent AS user_agent",
                           date = date,
                           table = "WikipediaPortal_14377354",
                           conditionals = "((event_cohort IS NULL) OR (event_cohort IN ('null','baseline')))")
  return(data)
}))
library(magrittr)
events$date %<>% lubridate::ymd()
events$ts %<>% lubridate::ymd_hms()
events <- events[order(events$date, events$session, events$ts), ]
events <- events[!duplicated(events[, c("session", "type")], fromLast = TRUE), ]
events$portal <- ifelse(events$date < "2016-03-10", "old", "new")
readr::write_tsv(events, "portal_events.tsv", append = file.exists("portal_events.tsv"))
q(save = "no")
