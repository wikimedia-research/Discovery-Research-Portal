start_date <- Sys.Date()-8 # as.Date("2016-01-17")
end_date <- Sys.Date()-1
events <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching Portal EL data from ", as.character(date), "\n")
  data <- wmf::build_query("SELECT LEFT(timestamp, 8) AS date,
                            event_session_id AS session,
                            event_destination AS destination,
                            event_event_type AS type,
                            event_section_used AS section_used,
                            event_accept_language AS preferred_languages,
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
events$type_id <- ifelse(events$type == "landing", 0, 1)
events <- events[order(events$date, events$session, events$type_id), ]
events$type_id <- NULL
events <- events[!duplicated(events[, c("session", "type")], fromLast = TRUE), ]
events$device <- uaparser::parse_agents(events$user_agent, fields = "device")
events <- events[events$device != "Spider", ]
events <- events[, setdiff(names(events), c("user_agent", "device"))]

import::from(dplyr, group_by, summarize, ungroup, mutate, rename, keep_where = filter, tbl_df)

sessions <- events %>%
  group_by(date, session) %>%
  summarize(destination = tail(destination, 1),
            clickthrough =  all(c("landing", "clickthrough") %in% type),
            section = tail(section_used, 1),
            preferred_languages = head(preferred_languages, 1)) %>%
  ungroup

# install.packages(c("NLP", "purrr"))
library(purrr)
accept_language <- sessions$preferred_languages %>%
  strsplit(",") %>%
  map_df(.f = function(lang_id) {
    langs <- tryCatch(unname(unlist(NLP::parse_IETF_language_tag(lang_id, expand = TRUE))),
                      error = function(e) { return(NA) }, finally = NA)
    if (is.na(langs[1])) {
      return(data.frame(V1 = NA, V2 = NA, V3 = NA, V4 = NA))
    }
    num_langs_pre_en <- which(langs == "English") - 1
    return(data.frame(V1 = ifelse(length(num_langs_pre_en) == 0, NA, num_langs_pre_en),
                      V2 = langs[1],
                      V3 = length(num_langs_pre_en) > 0,
                      V4 = length(lang_id),
                      stringsAsFactors = FALSE))
  }, .id = NULL) %>%
  set_names(c("Number of languages preceeding English", "Primary language", "Includes English", "Number of Accept-Languages"))

sessions <- cbind(sessions, accept_language)
# rm(accept_language)

sessions %<>% keep_where(!is.na(`Primary language`)) %>% tbl_df

## Remotely:
readr::write_rds(sessions, "portal_sessions.rds", compress = "gz")
## Locally:
# $> mkdir ~/Documents/Projects/Discovery\ Research/Portal\ Browser\ Preferred\ Languages/data
# $> scp stat2:/home/bearloga/portal_sessions.rds ~/Documents/Projects/Discovery\ Research/Portal\ Browser\ Preferred\ Languages/data/
