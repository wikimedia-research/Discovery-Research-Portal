end_date <- as.Date("2016-07-31")
start_date <- as.Date("2016-06-01")
events <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching Portal EL data from ", as.character(date), "\n")
  data <- wmf::build_query("SELECT
                             event_session_id AS session_id,
                             event_event_type AS type,
                             event_section_used AS section_used,
                             timestamp AS ts,
                             userAgent AS user_agent,
                             event_country AS country,
                             event_accept_language AS preferred_languages,
                             event_referer AS referrer",
                           date = date, table = "WikipediaPortal_14377354",
                           conditionals = "((event_cohort IS NULL) OR (event_cohort IN ('null','baseline')))")
  return(data)
})) # 863933 rows
library(magrittr)
events$ts %<>% lubridate::ymd_hms()

events$identity <- as.numeric(factor(paste0(events$session_id, events$user_agent, events$country, events$preferred_languages)))

# parse user agent; took a long time to run
library(uaparser)
events <- cbind(events, parse_agents(events$user_agent, fields=c("device", "os", "browser")))

library(tidyr)
import::from(dplyr, group_by, summarize, ungroup, mutate, rename, keep_where = filter, tbl_df, arrange)

events %<>%
  group_by(identity) %>%
  arrange(ts) %>%
  mutate(visit = cumsum(type == "landing")) %>%
  ungroup %>%
  keep_where(visit > 0) # 860109 rows

preferred_langs <- dplyr::distinct(events[, c("identity", "preferred_languages")])
events$preferred_languages <- NULL 

library(purrr)
accept_language <- preferred_langs$preferred_languages %>%
  strsplit(",") %>%
  map_df(.f = function(lang_id) {
    langs <- tryCatch(unname(unlist(NLP::parse_IETF_language_tag(lang_id, expand = TRUE))),
                      error = function(e) { return(NA) }, finally = NA)
    if (length(langs) == 0) {
      return(data.frame(V1 = NA, V2 = NA, V3 = NA, V4 = NA))
    }
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
preferred_langs <- cbind(preferred_langs, accept_language)
rm(accept_language)

# save.image("portal-session-lengths.RData")

events <- dplyr::left_join(events[, c("identity", "session_id",
                                      "user_agent", "country", "referrer",
                                      "ts", "visit", "type", "section_used",
									  "device", "os", "browser")],
                           preferred_langs, by = "identity")

readr::write_rds(events, "portal-session-lengths.rds", "gz")

dir.create("data")
system("scp stat2:/home/chelsyx/portal-session-lengths.rds data/")
