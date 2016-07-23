# This script describes the deployments we wish to assess.
# See: https://www.mediawiki.org/wiki/Wikipedia.org_Portal

deployments <- data.frame(
  Date = c(
    as.Date("2016-03-10"),
    as.Date("2016-06-02"),
    as.Date("2016-05-18"),
    as.Date("2016-07-04")#,
    # as.Date("????-??-??")
  ),
  Deployment = c(
    "Search Box with Type-ahead & Metadata",
    "Detect Language & Resort Primary Links",
    "Sister Project Links Descriptive Text",
    "RfC for new page design"#,
    # "Collapse Other Languages"
  ),
  Phabricator = c(
    "T125472",
    "T133432",
    "T133732",
    NA#,
    # T131526
  ),
  stringsAsFactors = FALSE
)

readr::write_csv(deployments, "data/portal_deployments.csv")
