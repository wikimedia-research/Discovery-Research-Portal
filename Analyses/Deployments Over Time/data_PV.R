# devtools::install_github("wikimedia/wikimedia-discovery-polloi")
portal_pageviews <- polloi::read_dataset("portal/portal_pageviews.tsv")
names(portal_pageviews) <- c("Date", "wikipedia.org")
readr::write_csv(portal_pageviews, "data/portal_pageviews.csv")
