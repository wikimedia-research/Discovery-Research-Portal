source("refine.R")

library(ggplot2)

temp <- pageviews[prefix == "uk" & project == "wikipedia",
                  list(pageviews = sum(pageviews)),
                  by = c("date", "page", "from", "speaks")]

{
  ggplot(temp, aes(x = date, y = pageviews, color = speaks)) +
  geom_line(size = 1.01) +
  facet_grid(page ~ from) +
  scale_y_log10("Pageviews", labels = polloi::compress) +
  scale_color_brewer("Browser language preferences", palette = "Set1", guide = guide_legend(ncol = 1)) +
  scale_x_date("Date", date_breaks = "2 week", date_labels = "%d %b") +
  geom_vline(xintercept = as.numeric(as.Date("2016-08-16")), linetype = "dashed") +
  ggthemes::theme_base(base_size = 12, base_family = "Helvetica Neue") +
  theme(legend.position = "bottom", rect = element_rect(color = NA),
        panel.border = element_rect(colour = "black")) +
  ggtitle("Pageviews to Ukrainian Wikipedia main page and other pages",
          subtitle = "Dashed line represents August 16th deployment of the secondary link collapse on Wikipedia.org Portal")
  } %>% ggsave("pageviews.png", plot = ., path = figures_dir,
               width = 15, height = 5, dpi = 300)
