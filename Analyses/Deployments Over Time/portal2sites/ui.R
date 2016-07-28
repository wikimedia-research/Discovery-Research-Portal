library(magrittr)
library(tidyr)
import::from(dplyr, group_by, summarize, ungroup, arrange, mutate, rename, select, left_join, keep_where = filter)
library(shiny)
library(shinythemes)
library(dygraphs)

deployments <- readr::read_csv("data/portal_deployments.csv") %>% arrange(Date)
deployments$Order <- 1:nrow(deployments)
el_destinations <- readr::read_csv("data/el_destinations.csv") %>%
  select(-c(`other languages`, `language search`)) %>%
  mutate(destination = sub(".org/", ".org", destination, fixed = TRUE)) %>%
  mutate(destination = sub("www.", "", destination))
el_destinations$Date %<>% as.Date
destinations <- el_destinations %>%
  gather(section_used, clicks, -c(Date, destination)) %>%
  group_by(destination, Date) %>%
  summarize(clicks = sum(clicks)) %>%
  summarize(clicks = mean(clicks)) %>%
  keep_where(!grepl("^[/\\?]", destination) & !grepl("\\.php|(\\.com)", destination)) %>%
  arrange(desc(clicks))
destination_choices <- as.list(destinations$destination)
names(destination_choices) <- sprintf("%s (%.1f)", destinations$destination, destinations$clicks)

fluidPage(theme = shinytheme("cosmo"),
  
  titlePanel("Sites visited from Wikipedia.org Portal"),
  
  fluidRow(column(selectInput("destination", "Site (Avg. Clicks Per Day)",
                              choices = destination_choices,
                              multiple = TRUE, selected = "en.wikipedia.org"),
                  helpText("Tip: you select multiple sites."),
                  width = 4),
           column(fluidRow(column(numericInput("roll", "Rolling period (smoothness)", 7, min = 1, max = 120), width = 6),
                           column(radioButtons("metric", "Metric to show", choices = list("Counts" = "count", "Proportions" = "prop"), inline = TRUE), width = 6)),
                  width = 8)),
  
  fluidRow(dygraphOutput("plot", height = "600px")),
  
  fluidRow(div(id = "legend", style = "text-align: center;"))
  
)
