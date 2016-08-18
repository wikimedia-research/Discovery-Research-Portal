library(data.table)
library(ggplot2)

webrequests <- readr::read_rds("data/portal_webrequest_counts_20160617-20160817_refined.rds")
pageviews <- readr::read_rds("data/portal_pageviews.rds")

pageviews_recounted <- webrequests[
  webrequests$http_status %in% c(200, 304) & webrequests$agent_type == "user" & webrequests$referer_class != "unknown",
  list("recounted" = sum(web_requests)),
  by = "date"]

constants <- dplyr::inner_join(pageviews, pageviews_recounted, by = "date") %>%
  dplyr::mutate(factor = recounted/pageviews)

set.seed(0)
bootstrapped_samples <- 2e3
p <- progress_estimated(bootstrapped_samples)
corrected_pageviews <- replicate(bootstrapped_samples, {
  sample(constants$factor, size = nrow(pageviews), replace = TRUE)
}, simplify = FALSE) %>%
  lapply(function(boostrapped_constants) {
    p$tick()$print()
    data.frame(date = pageviews$date,
               corrected = pageviews$pageviews * boostrapped_constants)
  }) %>%
  dplyr::bind_rows(.id = "replication") %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(lower = quantile(corrected, 0.025),
                   middle = median(corrected),
                   upper = quantile(corrected, 0.975))
p$stop()

{
  pageviews %>%
  dplyr::rename(original = pageviews) %>%
  dplyr::left_join(pageviews_recounted, by = "date") %>%
  tidyr::gather("Type of Pageviews", "pageviews", -date) %>%
  ggplot() +
  geom_ribbon(data = corrected_pageviews, aes(x = date, ymin = lower, ymax = upper),
              fill = RColorBrewer::brewer.pal(3, "Set1")[3], alpha = 0.25) +
  geom_line(data = corrected_pageviews, aes(x = date, y = middle),
            color = RColorBrewer::brewer.pal(3, "Set1")[3], alpha = 0.8) +
  geom_line(aes(x = date, y = pageviews, color = `Type of Pageviews`)) +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  scale_color_brewer(palette = "Set1",
                     limits = c("original", "recounted", "simulated"),
                     guide = guide_legend(nrow = 1)) +
  geom_vline(xintercept = as.numeric(min(webrequests$date)), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(max(webrequests$date)-1), linetype = "dashed") +
  ggthemes::theme_tufte(base_size = 12, base_family = "Gill Sans") +
  theme(panel.grid = element_line(color = "gray70"),
        panel.grid.major = element_line(color = "gray60"),
        legend.position = "bottom") +
  ggtitle("Wikipedia.org Portal Pageviews (PVs), Recounted PVs, and Bootstrapped PV Recounts",
          subtitle = "Recounted PVs include requests with HTTP status codes 200 & 304 only and exclude known spiders & unclassifiable referers")
} %>% ggsave("recounted.png", plot = ., path = "figures",
             width = 12, height = 6, dpi = 150)
