source("config.R")
library(data.table)
library(bsts)
library(magrittr)
library(ggplot2)

pageviews <- readr::read_tsv("data/pageview_counts_portal-ukwiki_20160805-20160825.tsv", col_types = "Dccccclllil")
pageviews <- dplyr::left_join(pageviews, polloi::get_prefixes()[, c("language", "prefix")], by = "prefix")
pageviews <- as.data.table(pageviews)

pageviews$post_deployment_indicator <- as.numeric(pageviews$post_deployment)
pageviews$site = paste(tolower(pageviews$language), pageviews$project)
pageviews$from <- ifelse(pageviews$from_wikipedia_portal, "from Wikipedia.org Portal", "from elsewhere")
pageviews$from[pageviews$from_search_engine] <- "from a search engine"
pageviews$from[pageviews$referer_class == "none"] <- "direct (e.g. bookmark or homepage)"
pageviews$from[pageviews$referer_class == "internal" & !pageviews$from_wikipedia_portal] <- "from a Wikimedia project/tool/bot"
pageviews$source <- paste(pageviews$site, pageviews$from)
pageviews$valid <- !(pageviews$project != "wikipedia" & pageviews$from_wikipedia_portal) & (pageviews$prefix %in% c("uk", "ru", "de", "en"))

# ukwiki <- pageviews[prefix %in% c("uk", "ru") & project == "wikipedia" & from_wikipedia_portal == TRUE,
#                     list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
#                     by = c("date", "prefix")] %>%
#   tidyr::spread(prefix, pageviews) %>%
#   dplyr::mutate(ru = sqrt(ru)) %>% # transform to a more similar scale
#   { xts::xts(.[, -1], order.by = .$date) }

# devtools::install_github("google/CausalImpact", force = TRUE)
# install.packages("dtw")
# devtools::install_github("klarsen1/MarketMatching", force = TRUE)
library(MarketMatching)

market_match <- pageviews[valid == TRUE, list(pageviews = sum(pageviews)), by = c("date", "source")] %>%
  tidyr::spread(source, pageviews, fill = 0) %>%
  tidyr::gather(source, pageviews, -date) %>%
  best_matches(id_variable = "source",
               date_variable = "date",
               matching_variable = "pageviews",
               parallel = TRUE, matches = 20,
               warping_limit = 1, # Default is 1, which means that a single query value can be mapped to at most 2 reference values.
               dtw_emphasis = 1, # rely only on dtw for pre-screening
               start_match_period = "2016-08-05",
               end_match_period = "2016-08-16")

# library(CausalImpact)
# causal_impact <- inference(matched_markets = market_match,
#                            test_market = "ukrainian wikipedia from Wikipedia.org Portal",
#                            end_post_period = "2016-08-25")
# causal_impact$PlotActualVersusExpected

# best_matches <- dplyr::filter(market_match$BestMatches, source == "ukrainian wikipedia from Wikipedia.org Portal")$BestControl
best_matches <- NULL

ukwiki <- pageviews[source %in% c("ukrainian wikipedia from Wikipedia.org Portal",
                                  "russian wikipedia from Wikipedia.org Portal",
                                  "ukrainian wikipedia from elsewhere",
                                  best_matches),
                    list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                    by = c("date", "source")] %>%
  tidyr::spread(source, pageviews, fill = 0) %>%
  { xts::xts(.[, -1, with = FALSE], order.by = .$date) }

ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`)
ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                  nseasons = 7, season.duration = 1) # Weekly seasonality
ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                  nseasons = 4, season.duration = 7) # Monthly seasonality

n_iters <- 1e4
burn_in <- 1e3
model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
              family = "gaussian", bma.method = "SSVS",
              state.specification = ss,
              niter = burn_in + n_iters, seed = 0)

summary(model)
plot(model, "state")
plot(model, "coefficients") # Inclusion probability

fit <- rowSums(aperm(model$state.contributions, c(1, 3, 2)), dims = 2)[-(1:burn_in), ]
colnames(fit) <- as.character(zoo::index(model$original.series))
fit <- as.data.frame(fit) %>%
  cbind(iteration = 1:n_iters) %>%
  tidyr::gather(date, value, -iteration) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(point_estimate = median(value),
                   lower_95 = quantile(value, 0.025),
                   upper_95 = quantile(value, 0.975),
                   lower_80 = quantile(value, 0.1),
                   upper_80 = quantile(value, 0.9)) %>%
  dplyr::left_join(data.frame(date = zoo::index(model$original.series),
                              actual = as.numeric(model$original.series),
                              stringsAsFactors = FALSE),
                   by = "date") %>%
  dplyr::mutate(sans = actual - mean(model$coefficients[-(1:burn_in), "post_deployment"]) * as.numeric(date >= "2016-08-16"))

MAPE <- fit %>%
  # dplyr::filter(date < "2016-08-15") %>%
  dplyr::summarize(mape = mean(abs(actual - point_estimate)/actual)) %>%
  { .$mape }

ggplot(fit, aes(x = date)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), alpha = 0.1, fill = "cornflowerblue") +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), alpha = 0.25, fill = "cornflowerblue") +
  geom_line(aes(y = point_estimate), color = "cornflowerblue", size = 2) +
  geom_line(aes(y = actual)) + geom_point(aes(y = actual)) +
  geom_line(aes(y = sans), color = "red", linetype = "dashed") +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_x_date("Date", date_breaks = "1 week", date_labels = "%d %b") +
  geom_vline(xintercept = as.numeric(as.Date("2016-08-16")), linetype = "dashed") +
  ggtitle(paste("Pageviews to", tolower(gsub("`", "", dimnames(attr(model$terms, "factors"))[[1]][1]))),
          subtitle = sprintf("Bayesian Structural Time Series model with mean absolute percentage error %.2f%%", 100 * MAPE)) +
  ggthemes::theme_tufte(12, "Gill Sans")
