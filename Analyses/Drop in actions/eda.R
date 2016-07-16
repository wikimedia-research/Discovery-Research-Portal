library(magrittr)
library(tidyr)
import::from(dplyr, group_by, summarize, keep_where = filter,
             arrange, ungroup, select, rename, mutate, tally)
library(ggplot2)
library(cowplot)
import::from(ggthemes, theme_tufte)

dashboard <- readr::read_rds("data/dashboard-breakdown-data.rds")
events <- readr::read_rds("data/portal-T139109-data.rds")

ggplot(dashboard, aes(x = date, y = proportion, color = section_used)) +
  geom_line(size = 1.1) +
  scale_y_continuous("Proportion", labels = scales::percent_format()) +
  geom_vline(xintercept = as.numeric(lubridate::ymd("2016-06-17")), linetype = "dashed") +
  tufte(base_size = 12, base_family = "Gill Sans") +
  labs(title = "Portal Action Breakdown", subtitle = "Downloaded from datasets.wikimedia.org")
