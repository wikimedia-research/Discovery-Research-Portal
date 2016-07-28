library(magrittr)
library(tidyr)
import::from(dplyr, group_by, summarize, ungroup, arrange, mutate, rename, select, left_join, keep_where = filter)
library(xts)
library(shiny)
library(dygraphs)

source("functions.R")

deployments <- readr::read_csv("data/portal_deployments.csv") %>% arrange(Date)
deployments$Order <- 1:nrow(deployments)
el_destinations <- readr::read_csv("data/el_destinations.csv") %>%
  select(-c(`other languages`, `language search`)) %>%
  mutate(destination = sub(".org/", ".org", destination, fixed = TRUE)) %>%
  mutate(destination = sub("www.", "", destination))
el_destinations$Date %<>% as.Date

function(input, output) {
  output$plot <- renderDygraph({
    el_data <- el_destinations[el_destinations$destination %in% input$destination, ]
    if (length(input$destination) > 1) {
      el_data <- el_data %>%
        gather(section_used, clicks, -c(Date, destination)) %>%
        group_by(Date, destination) %>%
        summarize(clicks = sum(clicks)) %>%
        spread(destination, clicks, fill = 0)
    } else {
      el_data <- select(el_data, -destination)
    }
    el_data %>%
    { xts(.[, -1], order.by = .$Date) } %>%
    {
      if (input$metric == "prop") {
        100 * ./rowSums(.)
      } else {
        .
      }
    } %>%
      dygraph(xlab = "Date", ylab = ifelse(input$prop, "Proportion", "Events"),
              main = paste0(input$destination, collapse = ", ")) %>%
      dyOptions(colors = smart_palette(ncol(el_data) - 1)) %>%
      dyLegend(labelsDiv = "legend") %>%
      dyRoller(rollPeriod = input$roll) %>%
      dyRangeSelector %>%
      dyEvent(as.character(deployments$Date[1]), deployments$Deployment[1], labelLoc = "bottom") %>%
      dyEvent(as.character(deployments$Date[2]), deployments$Deployment[2], labelLoc = "bottom") %>%
      dyEvent(as.character(deployments$Date[3]), deployments$Deployment[3], labelLoc = "bottom") %>%
      dyEvent(as.character(deployments$Date[4]), deployments$Deployment[4], labelLoc = "bottom")
  })
}
