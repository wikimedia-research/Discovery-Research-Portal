source("cda.R")

pageviews <- dplyr::left_join(readr::read_csv("data/pageviews_all_all.csv"),
                              readr::read_csv("data/portal_pageviews.csv"),
                              by = "Date") %>% dplyr::filter(Date >= "2015-11-20")

ts_data <- pageviews
names(ts_data) <- c("Date", "FR", "DE", "EN", "RU", "y")
ts_data <- ts_data[, c("Date", "y", "FR", "DE", "EN", "RU")]
ts_data[, 2:6] <- ts_data[, 2:6]/1e6

impacts <- list()
for (i in 1:(nrow(deployments)-1)) {
  cat("Fitting a causal impact model to Deployment #", i, " (", deployments$Deployment[i], ")...", sep = "")
  impact_pageviews <- CausalImpact(ts_data[, -1],
                                   pre.period = range(which(ts_data$Date < deployments$Date[i])),
                                   post.period = range(which(ts_data$Date >= deployments$Date[i] & ts_data$Date < deployments$Date[i+1])),
                                   model.args = list(niter = 1e4, standardize.data = TRUE,
                                                     nseasons = 7, season.duration = 1))
  cat("done.\n")
  impacts[[deployments$Deployment[i]]] <- impact_pageviews
}; rm(i, impact_pageviews)

pv_plot <- pageviews %>%
  dplyr::select(Date, wikipedia.org) %>%
  ggplot(aes(x = Date, y = wikipedia.org)) +
  geom_line(color = RColorBrewer::brewer.pal(3, "Set1")[1], size = 1.1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous("Pageviews", breaks = seq(1e7, 4.1e7, 5e6),
                labels = polloi::compress) +
  my_theme() +
  ggtitle("Wikipedia.org Portal Pageviews and Deployments") +
  geom_vline(data = deployments, aes(xintercept = as.numeric(Date)),
             linetype = "dashed") +
  geom_text(data = deployments, aes(x = Date, y = 1.7e7, label = Deployment),
            angle = 90, hjust = "left", vjust = "bottom", nudge_x = -2)

impact_plots <- lapply(names(impacts), function(deployment_name) {
  
  impact_summary <- impacts[[deployment_name]]$summary["Average", c("AbsEffect", "AbsEffect.lower", "AbsEffect.upper", "RelEffect", "RelEffect.lower", "RelEffect.upper")]
  impact_mcmc <- as.data.frame(impacts[[deployment_name]]$series[, c("response", "point.pred", "point.pred.lower", "point.pred.upper")])
  impact_mcmc$Date <- pageviews$Date
  impact_mcmc <- impact_mcmc[1:max(which(!is.na(impacts[[deployment_name]]$series[, "point.pred"]))), ]
  
  ggplot(impact_mcmc, aes(x = Date, y = response)) +
    geom_ribbon(aes(ymin = point.pred.lower, ymax = point.pred.upper), fill = "cornflowerblue", alpha = 0.25) +
    geom_line(aes(y = point.pred), color = "cornflowerblue") +
    geom_line() +
    scale_x_date(limits = range(pageviews$Date), date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous("Pageviews (in millions)", limits = c(0, 40), breaks = seq(0, 40, 10)) +
    geom_vline(xintercept = as.numeric(deployments$Date[deployments$Deployment == deployment_name]), linetype = "dashed") +
    ggtitle(deployment_name, subtitle = sprintf("%.2fM/%.2f%% (%.2fM, %.2fM)/(%.2f%%, %.2f%%) %s pageviews on average", impact_summary['AbsEffect'], 100 * impact_summary['RelEffect'], impact_summary['AbsEffect.lower'], impact_summary['AbsEffect.upper'], 100 * impact_summary['RelEffect.lower'], 100 * impact_summary['RelEffect.upper'], ifelse(impact_summary['RelEffect'] > 0, "more", "less"))) +
    my_theme()
  
})

plot_grid(plotlist = c(list(pv_plot), impact_plots), nrow = 2) %>%
  ggsave("impacts_pageviews.png", plot = ., path = "figures",
         units = "in", width = 15, height = 9, dpi = 300)

summaries <- lapply(names(impacts), function(deployment_name) {
  impact_summary <- capture.output(summary(impacts[[deployment_name]], "report", digits = 5))
  paste0("### ", deployment_name, "\n\n", paste0(impact_summary, collapse = "\n"))
}) %>% paste0(collapse = "\n\n") %>% gsub("Analysis report {CausalImpact}\n\n\n", "", ., fixed = TRUE)

par(mfrow = c(2, 2))
for (i in 1:length(impacts)) {
  plot(impacts[[i]]$model$bsts.model, "coefficients",
       ylab = "Predictor", main = names(impacts)[i])
  title(sub = "Posterior probability of each predictor being included in the model")
}; par(mfrow = c(1, 1))
