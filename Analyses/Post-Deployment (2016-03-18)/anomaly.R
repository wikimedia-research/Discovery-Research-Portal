library(magrittr)

devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

clickthroughs <- polloi::read_dataset("portal/clickthrough_rate.tsv", col_types = "Tci") %>%
  tidyr::spread(type, events) %>%
  dplyr::mutate(rate = clickthrough/landing)
# Time series as a two column data frame where:
#   - the first column consists of the timestamps
#   - the second column consists of the observations

res = AnomalyDetectionTs(clickthroughs[, c("date", "rate")],
                         max_anoms = 0.02, direction = 'both',
                         plot = TRUE)
res$plot
