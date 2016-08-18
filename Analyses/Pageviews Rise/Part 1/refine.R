library(data.table)
library(magrittr)

webrequests <- readr::read_rds("data/portal_webrequest_counts_20160617-20160817.rds")

webrequests$http_message <- as.character(do.call(rbind, lapply(webrequests$http_status, httr::http_status))[, "message"])

webrequests$is_pageview <- webrequests$is_pageview == "true"
sum(webrequests$is_pageview) # 0
webrequests$is_pageview <- NULL

readr::write_rds(webrequests, "data/portal_webrequest_counts_20160617-20160817_refined.rds", "gz")
