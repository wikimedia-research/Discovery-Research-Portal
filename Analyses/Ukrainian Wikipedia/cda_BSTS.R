source("config.R")
library(data.table)
library(bsts)
library(magrittr)
library(ggplot2)
library(dygraphs)

# pageviews <- readr::read_tsv("data/pageview_counts_portal-ukwiki_20160805-20160825.tsv", col_types = "Dccccclllil")
pageviews <- readr::read_tsv("data/pageview_counts_portal-ukwiki_20160629-20160828.tsv", col_types = "Dccccclllil")
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

## TO-DO:
# - Roll up all ukwiki pageviews not from portal into a single group ("not from Wikipedia.org Portal")
# - Try out additional models where "ukwiki pvs not from Wikipedia.org Portal" is a predictor

market_match <- pageviews[valid == TRUE, list(pageviews = sum(pageviews)), by = c("date", "source")] %>%
  tidyr::spread(source, pageviews, fill = 0) %>%
  tidyr::gather(source, pageviews, -date) %>%
  best_matches(id_variable = "source",
               date_variable = "date",
               matching_variable = "pageviews",
               parallel = TRUE, matches = 20,
               warping_limit = 1, # Default is 1, which means that a single query value can be mapped to at most 2 reference values.
               dtw_emphasis = 1, # rely only on dtw for pre-screening
               start_match_period = as.character(min(pageviews$date)),
               end_match_period = "2016-08-15")

best_matches <- dplyr::filter(market_match$BestMatches, source == "ukrainian wikipedia from Wikipedia.org Portal")$BestControl

market_match$BestMatches %>%
  dplyr::filter(source == "ukrainian wikipedia from Wikipedia.org Portal") %>%
  dplyr::select(BestControl, RelativeDistance, Correlation) %>%
  dplyr::rename(Control = BestControl,
                Distance = RelativeDistance) %>%
  knitr::kable(format = "markdown", digits = 3)

n_iters <- 1e4
burn_in <- 1e3

make_data_bsts_ready <- function(data) {
  return({
    data %>%
    tidyr::spread(source, pageviews, fill = 0) %>%
    { xts::xts(.[, -1, with = FALSE], order.by = .$date) } %>%
    { .[, union("ukrainian wikipedia from Wikipedia.org Portal", names(.))] }
  })
}

make_prior <- function(data) {
  prior_inclusion <- rep(0, ncol(data))
  prior_inclusion[names(data) == "post_deployment"] <- 1
  prior_inclusion[names(data) %in% c(
    "russian wikipedia from Wikipedia.org Portal",
    "ukrainian wikipedia from elsewhere")] <- 1
  prior_inclusion[names(data) %in% c(
    "russian wikibooks from elsewhere",
    "ukrainian wiktionary from a search engine")] <- 0.5
  ## Unnecessary because of (Intercept) term:
  # prior_inclusion <- prior_inclusion[-which(names(data) == "ukrainian wikipedia from Wikipedia.org Portal")]
  # names(prior_inclusion) <- names(data)[-1]
  prior <- SpikeSlabPrior(x = model.matrix(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = data),
                          y = data$`ukrainian wikipedia from Wikipedia.org Portal`,
                          prior.inclusion.probabilities = prior_inclusion)
  return(prior)
}

model_1 <- (function() {

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia from Wikipedia.org Portal", # response
                                    best_matches),
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`),
                niter = burn_in + n_iters, seed = 0)
  return(model)

})()

model_2 <- (function() {

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia from Wikipedia.org Portal", # response
                                    "russian wikipedia from Wikipedia.org Portal", # 1.7% incl. prob.
                                    "ukrainian wikipedia from elsewhere",
                                    "russian wikibooks from elsewhere", # 6% incl. prob.
                                    "ukrainian wiktionary from a search engine"), # 3% incl. prob.
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`),
                niter = burn_in + n_iters, seed = 0)
  return(model)

})()

model_3 <- (function() {

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia from Wikipedia.org Portal", # response
                                    best_matches),
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

})()

model_4 <- (function() {

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia from Wikipedia.org Portal", # response
                                    "russian wikipedia from Wikipedia.org Portal", # 1.7% incl. prob.
                                    "ukrainian wikipedia from elsewhere",
                                    "russian wikibooks from elsewhere", # 6% incl. prob.
                                    "ukrainian wiktionary from a search engine"), # 3% incl. prob.
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

})()

model_5 <- (function() {

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia from Wikipedia.org Portal", # response
                                    "russian wikipedia from Wikipedia.org Portal", # 1.7% incl. prob.
                                    "ukrainian wikipedia from elsewhere"),
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

})()

model_6 <- (function() {

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia from Wikipedia.org Portal", # response
                                    "russian wikipedia from Wikipedia.org Portal", # 1.7% incl. prob.
                                    "ukrainian wikipedia from elsewhere"),
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

})()

model_7 <- (function() {

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia from Wikipedia.org Portal", # response
                                    "russian wikipedia from Wikipedia.org Portal"),
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

})()

model_8 <- (function() {

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia from Wikipedia.org Portal", # response
                                    "ukrainian wikipedia from elsewhere"),
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

})()

model_9 <- (function() {

  ukwiki <- pageviews[source == "ukrainian wikipedia from Wikipedia.org Portal",
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

})()

model_10 <- (function() {

  ukwiki <- pageviews[source == "ukrainian wikipedia from Wikipedia.org Portal",
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

})()

model_11 <- (function() {

  ukwiki <- pageviews[source == "ukrainian wikipedia from Wikipedia.org Portal",
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`)
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

})()

model_12 <- (function() {

  ukwiki <- pageviews[source == "ukrainian wikipedia from Wikipedia.org Portal",
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia from Wikipedia.org Portal`),
                niter = burn_in + n_iters, seed = 0)
  return(model)

})()

model_list <- list(
  "Matched markets, no AR/seasonality" = model_1,
  "Mixed markets, no AR/seasonality" = model_2,
  "Matched markets, AR + seasonality" = model_3,
  "Mixed markets, AR + seasonality" = model_4,
  "Selected markets, no AR/seasonality" = model_5,
  "Selected markets, AR + seasonality" = model_6,
  "Ruwiki from Portal, AR + seasonality" = model_7,
  "Ukwiki from elsewhere, AR + seasonality" = model_8,
  "No markets, AR + seasonality" = model_9,
  "No markets, seasonality, no AR" = model_10,
  "No markets, AR, no seasonality" = model_11,
  "No markets, no AR/seasonality" = model_12
); rm(model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8, model_9, model_10, model_11, model_12)
prediction_errors <- matrix(0, nrow = length(unique(pageviews$date)), ncol = length(model_list))
colnames(prediction_errors) <- names(model_list)
for (model_name in names(model_list)) {
  prediction_errors[, model_name] <- cumsum(abs(colMeans(bsts.prediction.errors(model_list[[model_name]])[-(1:burn_in), , drop = FALSE])))
}; rm(model_name)
prediction_errors <- as.data.frame(prediction_errors)
prediction_errors$Date <- unique(pageviews$date)

prediction_errors %>%
  tidyr::gather(Model, `Cumulative Absolute Error`, -Date) %>%
  ggplot(aes(x = Date, y = `Cumulative Absolute Error`, color = Model)) +
  geom_line() +
  ggthemes::theme_tufte(12, "Gill Sans") +
  theme(legend.position = "bottom")

# prediction_errors %>%
#   { .[c(names(sort(tail(., 1)[, 1:length(model_list)], decreasing = TRUE)), "Date")] } %>%
#   # { .[c(names(model_list)[c(3, 4, 7, 8)], "Date")] } %>%
#   { xts::xts(.[, 1:(ncol(.)-1)], order.by = .$Date) } %>%
#   dygraph(main = "Comparison of BSTS Models", ylab = "Cumulative Absolute Error", xlab = "Date") %>%
#   dyOptions(colors = RColorBrewer::brewer.pal(length(model_list), "Paired"), labelsKMB = TRUE) %>%
#   # dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"), labelsKMB = TRUE) %>%
#   dyLegend(labelsSeparateLines = TRUE, width = 400) %>%
#   dyRangeSelector(fillColor = "gray40", strokeColor = "black") %>%
#   dyCSS("dygraph.css")

par(mfrow = c(4, 3))
for (model_name in names(model_list)) {
  plot(model_list[[model_name]], "state", main = model_name)
}; par(mfrow = c(1, 1))

for (model_name in names(model_list)) {
  cat("\n==============\n")
  print(which(names(model_list) == model_name))
  print(model_name)
  print(sprintf("R2: %.2f | GoF: %.2f", summary(model_list[[model_name]], burn = burn_in)$rsquare, summary(model_list[[model_name]], burn = burn_in)$relative.gof))
  print(summary(model_list[[model_name]], burn = burn_in)$coefficients["post_deployment", ])
}

par(mfrow = c(3, 2))
for (model_name in names(model_list)[c(3, 4, 6, 7, 8, 9)]) {
  plot(model_list[[model_name]], "state", main = model_name)
}; par(mfrow = c(1, 1))

for (model_name in names(model_list)[c(3, 4, 6, 7, 8, 9)]) {
  cat("\n==============\n")
  print(model_name)
  print(sprintf("R2: %.2f | GoF: %.2f", summary(model_list[[model_name]], burn = burn_in)$rsquare, summary(model_list[[model_name]], burn = burn_in)$relative.gof))
  print(summary(model_list[[model_name]], burn = burn_in)$coefficients["post_deployment", ])
}

# Hacked-together log-likehood extraction for AIC and BIC calculation:
logLik.bsts <- function(object, ...) {
  n_hyperparams <- sum(grepl("(sigma|phi)", names(object))) + 2 # sigma's, phi's, and (mean + slope)
  n_gammas <- sum(as.numeric(sub("^sigma\\.seasonal\\.([0-9]+)\\.?[0-9]*", "\\1", names(object)[grepl("sigma.seasonal", names(object), fixed = TRUE)])))
  n_coefficients <- ncol(object$predictors)
  structure(median(object$log.likelihood),
            df = n_hyperparams + n_gammas + n_coefficients,
            nobs = length(object$original.series),
            class = "logLike")
}

save(list = c("market_match", "logLik.bsts", "model_list", "pageviews", "n_iters", "burn_in"),
     file = "data/bsts.RData")

model <- model_list[[6]]
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
  dplyr::mutate(deployment_effect_middle = median(model$coefficients[-(1:burn_in), "post_deployment"]) * as.numeric(date >= "2016-08-16"),
                deployment_effect_lower = quantile(model$coefficients[-(1:burn_in), "post_deployment"], 0.025) * as.numeric(date >= "2016-08-16"),
                deployment_effect_upper = quantile(model$coefficients[-(1:burn_in), "post_deployment"], 0.975) * as.numeric(date >= "2016-08-16"),
                sans = actual - deployment_effect_middle, sans_lower = actual - deployment_effect_lower, sans_upper = actual - deployment_effect_upper)

# MAPE <- fit %>%
#   # dplyr::filter(date < "2016-08-15") %>%
#   dplyr::summarize(mape = mean(abs(actual - point_estimate)/actual)) %>%
#   { .$mape }

ggplot(fit, aes(x = date)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), alpha = 0.1, fill = "cornflowerblue") +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), alpha = 0.25, fill = "cornflowerblue") +
  geom_line(aes(y = point_estimate), color = "cornflowerblue", size = 2) +
  geom_ribbon(aes(ymin = sans_lower, ymax = sans_upper), alpha = 0.25, fill = "red") +
  geom_line(aes(y = sans), color = "red", linetype = "dashed") +
  geom_line(aes(y = actual)) + geom_point(aes(y = actual)) +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_x_date("Date", date_breaks = "1 week", date_labels = "%d %b") +
  geom_vline(xintercept = as.numeric(as.Date("2016-08-16")), linetype = "dashed") +
  ggtitle(paste("Pageviews to", tolower(gsub("`", "", dimnames(attr(model$terms, "factors"))[[1]][1]))),
          # subtitle = sprintf("Bayesian Structural Time Series model with mean absolute percentage error %.2f%%", 100 * MAPE),
          subtitle = "Bayesian Structural Time Series model") +
  ggthemes::theme_tufte(12, "Gill Sans")

rm(model, fit)
