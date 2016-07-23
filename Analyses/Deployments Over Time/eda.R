library(magrittr)
library(tidyr)
import::from(dplyr, group_by, summarize, ungroup, arrange, mutate, rename, select, left_join, keep_where = filter)
library(ggplot2)
library(cowplot)
import::from(ggthemes, tufte = theme_tufte)

options(scipen = 500)

pageviews <- dplyr::left_join(readr::read_csv("data/pageviews_all_all.csv"),
                              readr::read_csv("data/portal_pageviews.csv"),
                              by = "Date")
deployments <- readr::read_csv("data/portal_deployments.csv") %>% arrange(Date)
deployments$Order <- 1:nrow(deployments)

pageviews %>%
  gather(Site, Pageviews, -Date) %>%
  ggplot(aes(x = Date, y = Pageviews, color = Site)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_log10(breaks = union(c(1e6, 1e7, 1.5e7, 2e7, 3e7, 7e7), seq(1e6, 3.5e8, 5e7)),
                labels = polloi::compress) +
  tufte(base_size = 12, base_family = "Gill Sans") +
  ggtitle("Pageviews") +
  geom_vline(data = deployments, aes(xintercept = as.numeric(Date)),
             linetype = "dashed") +
  geom_label(data = deployments, aes(x = Date, y = 5.1e7 + (Order-1) * 5e7, label = Deployment),
             color = "black", nudge_y = 0.05, fill = "white") +
  geom_point(data = deployments, aes(x = Date, y = 5.1e7 + (Order-1) * 5e7),
             color = "black")

el_daily <- readr::read_csv("data/el_daily_aggregates.csv")
el_daily$Date %<>% as.Date

el_daily %>%
  ggplot(aes(x = Date, y = sessions)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid.major = element_line(color = "gray80"), panel.grid.major.y = element_blank()) +
  geom_line() +
  ggtitle("Number of sampled sessions (visitors) over time",
          subtitle = "Users were selected for Event Logging at a sampling rate of 1:200") +
  geom_vline(data = deployments, aes(xintercept = as.numeric(Date)),
             linetype = "dashed") +
  geom_label(data = deployments, aes(x = Date, y = 1e3*Order, label = Deployment),
             color = "black", fill = "white")

el_daily_us <- readr::read_csv("data/el_daily_aggregates_us.csv")
el_daily_us$Date %<>% as.Date

el_daily_us %>%
  # mutate(Country = ifelse(is.na(Country), "(Not Available)", Country)) %>%
  ggplot(aes(x = Date, y = sessions, fill = Country)) +
  geom_area(position = "fill", color = "black") +
  scale_y_continuous("Proportion of sessions", labels = scales::percent_format()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ggtitle("Proportion of sessions from US vs Non-US",
          subtitle = "Gray refers to sessions where a country could not be determined.") +
  tufte(base_size = 12, base_family = "Gill Sans") +
  geom_vline(data = deployments, aes(xintercept = as.numeric(Date)),
             linetype = "dashed") +
  geom_label(data = deployments, aes(x = Date, y = 0.1*Order, label = Deployment),
             color = "black", fill = "white")

el_destinations <- readr::read_csv("data/el_destinations.csv")
el_destinations$Date %<>% as.Date
destinations <- el_destinations %>%
  select(-Date) %>%
  gather(section_used, clicks, -destination) %>%
  mutate(destination = sub(".org/", ".org", destination, fixed = TRUE)) %>%
  mutate(destination = sub("www.", "", destination)) %>%
  group_by(destination) %>%
  summarize(clicks = sum(clicks)) %>%
  keep_where(!grepl("^[/\\?]", destination) & !grepl("\\.php|(\\.com)", destination)) %>%
  arrange(desc(clicks))

library(shiny)
library(dygraphs)
shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(selectInput("destination", "Site visited from Wikipedia Portal", choices = destinations$destination, multiple = FALSE),
                   radioButtons("metric", "Metric to show", choices = list("Counts" = "count", "Proportions" = "prop")),
                   div(id = "legend")),
      mainPanel(dygraphOutput("plot"))
    )
  ), 
  server = function(input, output) {
    output$plot <- renderDygraph(
      el_destinations[el_destinations$destination %in% input$destination, ] %>%
        select(-destination) %>%
        { xts::xts(.[, -1], order.by = .$Date) } %>%
        {
          if (input$metric == "prop") {
            100 * ./rowSums(.)
          } else {
            .
          }
        } %>%
        dygraph %>%
        dyLegend(labelsDiv = "legend") %>%
        dyRoller(rollPeriod = 7) %>%
        dyRangeSelector %>%
        dyEvent(as.character(deployments$Date[1]), deployments$Deployment[1], labelLoc = "bottom") %>%
        dyEvent(as.character(deployments$Date[2]), deployments$Deployment[2], labelLoc = "bottom") %>%
        dyEvent(as.character(deployments$Date[3]), deployments$Deployment[3], labelLoc = "bottom") %>%
        dyEvent(as.character(deployments$Date[4]), deployments$Deployment[4], labelLoc = "bottom")
    )
  }
)
