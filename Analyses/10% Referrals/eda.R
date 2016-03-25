library(dplyr)

referrals %>%
  group_by(date, referer_class) %>%
  summarize(n = n()) %>%
  # mutate(n = 100*n/sum(n)) %>%
  mutate(n = sprintf("%.2f%%", 100*n/sum(n))) %>%
  ungroup %>%
  arrange(date, desc(n)) %>%
  # group_by(referer_class) %>%
  # summarize(median = median(n))
  tidyr::spread(referer_class, n) %>%
  knitr::kable(format = "markdown")

lapply(useless_requests_sets, function(x) {
  return(c(sum(x), 100*sum(x)/length(x)))
}) %>%
  do.call(rbind, .) %>%
  magrittr::set_colnames(c("Requests", "% of 10% Traffic")) %>%
  { .[order(.[, 'Requests'], decreasing = TRUE), ] } %>%
  knitr::kable(format = "markdown")

parsed %>%
  filter(referrals$referer_class == "internal") %>%
  group_by(domain) %>%
  summarize(n = n()) %>%
  mutate(n = 100*n/sum(n)) %>%
  filter(n < 0.015) %>%
  summarize(domain = n(), n = sum(n)) %>%
  # filter(n >= 0.015) %>%
  # arrange(desc(n)) %>%
  mutate(n = sprintf("%.3f%%", n)) %>%
  knitr::kable(format = "markdown")

head(parsed[!useless_requests_set, ], 20)
