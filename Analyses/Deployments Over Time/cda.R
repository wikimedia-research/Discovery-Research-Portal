# deployments <- readr::read_csv("data/portal_deployments.csv") %>% arrange(Date)
# data_range <- seq(as.Date("2015-11-11"), as.Date("2016-07-20"), "day")
# timeline <- as.data.frame(apply(deployments, 1, function(deployment) {
#   return(as.numeric(data_range >= deployment['Date']))
# }))
# names(timeline) <- deployments$Deployment

library(magrittr)
library(ggplot2); library(cowplot)
my_theme <- function() {
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
    theme(panel.grid = element_line(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(color = "gray60"),
          strip.background = element_rect(fill = "gray40", color = "gray40"),
          strip.text = element_text(color = "white"),
          panel.border = element_rect(color = "gray40", fill = NA))
}

## Google's Bayesian Structural Time Series package
library(bsts) # install.packages("bsts")

## Google's CausalImpact package
# Install: R> devtools::install_github("google/CausalImpact")
# Homepage: https://google.github.io/CausalImpact/
# Examples: https://google.github.io/CausalImpact/CausalImpact.html
library(CausalImpact)

deployments <- readr::read_csv("data/portal_deployments.csv") %>% dplyr::arrange(Date)
