library(tidyr)
import::from(dplyr, group_by, summarize, ungroup, mutate, rename, keep_where = filter, tbl_df, arrange)
library(urltools) # devtools::install_github("ironholds/urltools")

events <- readr::read_rds("data/portal-session-lengths.rds")

get_secs_to_first_click <- function(timestamp, event_type) {
  if (!"clickthrough" %in% event_type) {
    return(as.numeric(NA))
  } else {
    return(as.numeric(difftime(min(timestamp[event_type == "clickthrough"]), min(timestamp), units = "secs")))
  }
}
get_first_section <- function(sections) {
  if (sum(is.na(sections)) == length(sections)) {
    return(as.character(NA))
  } else {
    return(sections[!is.na(sections)][1])
  }
}
get_most_common_referrer <- function(referrer_urls) {
  if (sum(is.na(referrer_urls)) == length(referrer_urls)) {
    return(as.character(NA))
  } else {
    domains <- trimws(sub("^www1?\\.", "", domain(referrer_urls)), which = "both")
    return(names(sort(table(domains), decreasing = TRUE))[1])
  }
}
sessions <- events %>%
  group_by(identity) %>%
  arrange(ts) %>%
  summarize(ts_first_visit = head(ts, 1),
            country = head(country, 1),
			device = head(device, 1),
			os = head(os, 1),
			browser = head(browser, 1),
            clickthrough = "clickthrough" %in% type,
            total_clicks = sum(type == "clickthrough"),
            total_visits = max(visit),
            referred_visits = sum(!is.na(referrer)),
            sections_clicked = length(na.omit(unique(section_used))),
            secs_to_first_click = get_secs_to_first_click(ts, type),
            session_length = as.numeric(difftime(max(ts), min(ts), units = "secs")),
            session_length_first_visit = as.numeric(difftime(max(head(ts, 2)), min(ts), units = "secs")),
            most_common_referrer = get_most_common_referrer(referrer),
            first_clicked_section = get_first_section(section_used),
            preferred_languages = head(preferred_languages, 1),
            langs_preceeding_en = head(`Number of languages preceeding English`, 1),
            primary_language = head(`Primary language`, 1),
            includes_english = head(`Includes English`, 1),
            number_languages = head(`Number of Accept-Languages`, 1)) %>%
  ungroup %>%
  mutate(secs_to_first_click = ifelse(total_clicks == 0, as.numeric(NA), secs_to_first_click),
         most_common_referrer = ifelse(most_common_referrer == "", as.character(NA), most_common_referrer),
         most_common_referrer = sub("(google)\\..*", "\\1", most_common_referrer),
         most_common_referrer = sub("(yahoo)\\..*", "\\1", most_common_referrer),
         most_common_referrer = sub("(bing)\\..*", "\\1", most_common_referrer),
         most_common_referrer = sub("(facebook)\\..*", "\\1", most_common_referrer))

# library(reconstructr)
# events_by_user <- split(events$ts, events$identity)
# reconstructed_seshs <- reconstruct_sessions(events_by_user)
# reconstructed_lengths <- session_length(reconstructed_seshs)
# # bounce_rate(reconstructed_sessions, 4)
# reconstructed_sessions <- data.frame(identity = names(events_by_user),
#                                      reconstructed_length = ifelse(reconstructed_lengths < 0, as.numeric(NA), reconstructed_lengths),
#                                      events = session_events(reconstructed_seshs))
# sessions <- dplyr::left_join(sessions, reconstructed_sessions, by = "identity")

data("ISO_3166_1", package = "ISOcodes")
sessions <- dplyr::left_join(sessions, ISO_3166_1[, c("Alpha_2", "Name")], by = c("country" = "Alpha_2")) %>% rename(country_name = Name)

readr::write_rds(sessions, "data/portal-session-lengths-refined.rds", "gz")

rm(list = ls())
