source("cda.R")

pageviews <- dplyr::left_join(readr::read_csv("data/pageviews_all_all.csv"),
                              readr::read_csv("data/portal_pageviews.csv"),
                              by = "Date") %>% dplyr::filter(Date >= "2015-11-20")

el_ctr <- readr::read_csv("data/el_daily_most_common.csv") %>%
  dplyr::mutate(Date = as.Date(Date)) %>%
  dplyr::filter(Date >= "2015-11-20")

standardize <- function(x) {
  return((x - mean(x))/sd(x))
}
normalize <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}

pageviews %<>%
  dplyr::select(-wikipedia.org) %>%
  tidyr::gather(site, pvs, -Date) %>%
  dplyr::group_by(site) %>%
  dplyr::mutate(pvs_z = standardize(pvs), pvs_norm = normalize(pvs))

el_ctr %<>%
  dplyr::select(-proportion) %>%
  tidyr::spread(section_used, nn) %>%
  dplyr::select(-c(`other languages`, `language search`))

ts_data <- pageviews %>%
  dplyr::select(c(Date, site, pvs_norm)) %>%
  tidyr::spread(site, pvs_norm) %>%
  dplyr::left_join(el_ctr, by = "Date") %>%
  as.data.frame

# matplot(ts_data[, -1], type = "l")

sections <- list()

for ( section_used in c("primary links", "search", "secondary links", "other projects") ) {
  
  cat("Assessing causal impact via Bayesian structural time series model for sessions that most commonly clicked on", section_used, "\n")
  
  impacts <- list()
  for ( i in 1:(nrow(deployments)-1) ) {
    cat("Constructing a partially hidden dataset of Deployment #", i, " (", deployments$Deployment[i], ")...", sep = "")
    ## Construct the dataset, hiding the post-deployment response:
    ts_data_hidden <- ts_data[ts_data$Date < deployments$Date[i + 1], c("Date", section_used, "en.wikipedia.org", "de.wikipedia.org", "ru.wikipedia.org", "fr.wikipedia.org")]
    names(ts_data_hidden) <- c("Date", "y", "en.wikipedia.org", "de.wikipedia.org", "ru.wikipedia.org", "fr.wikipedia.org")
    Y <- ts_data_hidden$y
    Y[ts_data_hidden$Date >= deployments$Date[i]] <- NA
    ts_data_hidden$y <- Y
    ## Add seasonality components:
    ss <- AddLocalLinearTrend(y = Y)
    ss <- AddSeasonal(ss, Y, nseasons = 7, season.duration = 1) # Weekly seasonality
    ss <- AddSeasonal(ss, Y, nseasons = 4, season.duration = 7) # Monthly seasonality
    cat("done.\nFitting a BSTS model to Deployment #", i, " (", deployments$Deployment[i], ")...\n", sep = "")
    model_clickthrough <- bsts(y ~ en.wikipedia.org + fr.wikipedia.org + ru.wikipedia.org + de.wikipedia.org,
                               data = ts_data_hidden, state.specification = ss, niter = 1e4, seed = 0)
    cat("Done! Now on to generating a causal impact for Deployment #", i, " (", deployments$Deployment[i], ")...", sep = "")
    impact_clickthrough <- CausalImpact(bsts.model = model_clickthrough,
                                        post.period.response = ts_data[[section_used]][is.na(Y)])
    impacts[[deployments$Deployment[i]]] <- impact_clickthrough
    cat("done.\n")
  }; rm(i, ts_data_hidden, Y, ss, model_clickthrough, impact_clickthrough)
  
  sections[[section_used]] <- impacts
  
}; rm(section_used)

ctr_plot <- el_ctr %>%
  tidyr::gather(section, sessions, -Date) %>%
  ggplot(aes(x = Date, y = sessions, color = section)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 7),
              se = FALSE, alpha = 0.75) +
  geom_line(alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_log10("Sessions") +
  my_theme() + theme(legend.position = "bottom") +
  ggtitle("Number of Sessions Per Most Common Section Clicked",
          subtitle = "Number of sessions where each section was the most commonly clicked section across all the session's visits") +
  geom_vline(data = deployments, aes(xintercept = as.numeric(Date)),
             linetype = "dashed") +
  geom_text(data = deployments, aes(x = Date, y = 10, label = Deployment),
            angle = 90, hjust = "left", vjust = "bottom", nudge_x = -2, size = 3.5,
            color = "black") +
  scale_color_discrete(limits = c("search", "primary links", "secondary links", "other projects"))

cowplot::ggsave(
  "sections.png",
  plot = ctr_plot +
    theme(panel.grid.major.y = element_line(color = "gray80"),
          panel.grid.minor.y = element_line(color = "gray90")),
  path = "figures", units = "in", width = 12, height = 6, dpi = 300)

for ( section_used in names(sections) ) {
  lapply(names(sections[[section_used]]), function(deployment_name) {
    plot(sections[[section_used]][[deployment_name]], c("original")) +
      scale_y_continuous("Sessions") +
      ggtitle(paste("Impact on", section_used),
              subtitle = paste(deployment_name, "deployed on", deployments$Date[deployments$Deployment == deployment_name])) +
      my_theme()
  }) %>% c(list(ctr_plot), .) %>% plot_grid(plotlist = ., nrow = 2) %>% print
}

for ( section_used in names(sections) ) {
  
  ctr_plot <- el_ctr %>%
    ggplot(aes(x = Date)) +
    geom_line(aes_(y = as.name(section_used)), alpha = 0.5) +
    scale_y_log10("Sessions", limits = range(el_ctr[, -1])) +
    ggtitle(paste0("Sessions where \"", section_used, "\" was the most common section clicked on.")) +
    my_theme() +
    geom_vline(data = deployments, aes(xintercept = as.numeric(Date)),
               linetype = "dashed") +
    geom_text(data = deployments, aes(x = Date, y = 10, label = Deployment),
              angle = 90, hjust = "left", vjust = "bottom", nudge_x = -2, size = 3.5,
              color = "black")
  
  lapply(names(sections[[section_used]]), function(deployment_name) {
    
    impact_mcmc <- as.data.frame(sections[[section_used]][[deployment_name]]$series[, c("response", "point.pred", "point.pred.lower", "point.pred.upper")])
    impact_mcmc$Date <- el_ctr$Date[1:nrow(impact_mcmc)]
    ## MFE is the median forecast error: abs(observed - predicted), up to the date of deployment
    ## It shows us the median (absolute) difference. A MFE of 0.8% indicates that on average the
    ## observed value (e.g. 52.8%) was higher (or lower!) than the predicted value (e.g. 52%) by
    ## 0.8%. It lets us calibrate our own opinion of how accurate the prediction post-deployment
    ## is potentially, since that's the prediction we're using to assess the change's impact.
    impact_mcmc$Error <- abs(impact_mcmc$response - impact_mcmc$point.pred)
    impact_mcmc$Error[impact_mcmc$Date >= deployments$Date[deployments$Deployment == deployment_name]] <- NA
    ## MRFE is the median relative forecast error: (observed - predicted)/max(observed, predicted), up to the data of deployment
    # impact_mcmc$RelError <- abs(impact_mcmc$response - impact_mcmc$point.pred)/pmax(impact_mcmc$response, impact_mcmc$point.pred)
    # impact_mcmc$RelError[impact_mcmc$Date >= deployments$Date[deployments$Deployment == deployment_name]] <- NA
    ## MPPE is the median positive percent error: (|experimental| - |theoretical|)/|theoretical|, up to the date of deployment
    impact_mcmc$PerError <- abs((impact_mcmc$response - impact_mcmc$point.pred)/impact_mcmc$point.pred)
    impact_mcmc$PerError[impact_mcmc$Date >= deployments$Date[deployments$Deployment == deployment_name]] <- NA
    
    impact_summary <- sections[[section_used]][[deployment_name]]$summary["Average", c("Actual", "Pred")]
    
    ggplot(impact_mcmc, aes(x = Date, y = response)) +
      geom_ribbon(aes(ymin = point.pred.lower, ymax = point.pred.upper), fill = "cornflowerblue", alpha = 0.1) +
      geom_line(aes(y = point.pred), color = "cornflowerblue") +
      geom_line() +
      scale_x_date(limits = range(el_ctr$Date), date_breaks = "1 month", date_labels = "%b") +
      scale_y_continuous("Sessions") +
      geom_vline(xintercept = as.numeric(deployments$Date[deployments$Deployment == deployment_name]), linetype = "dashed") +
      my_theme() +
      ggtitle(paste0("Impact of ", deployment_name, " (", as.character(deployments$Date[deployments$Deployment == deployment_name], "%D"), ")"),
              subtitle = sprintf("%.0f actual vs %.0f predicted on avg.; as predicted by pageviews; w/ MFE of %.1f & MPPE of %.2f%%", impact_summary['Actual'], impact_summary['Pred'], median(impact_mcmc$Error, na.rm = TRUE), 100*median(impact_mcmc$PerError, na.rm = TRUE)))
    
  }) %>% c(list(ctr_plot), .) %>% plot_grid(plotlist = ., nrow = 2) %>%
    cowplot::ggsave(paste0("impacts_", section_used, ".png"), plot = ., path = "figures",
                    units = "in", width = 15, height = 9, dpi = 300)
   
}
