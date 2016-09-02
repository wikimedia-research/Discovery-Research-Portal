source("config.R")
library(data.table)
library(bsts)
library(magrittr)
library(ggplot2)
library(dygraphs)

# pageviews <- readr::read_tsv("data/pageview_counts_portal-ukwiki_20160805-20160825.tsv", col_types = "Dccccclllil")
# pageviews <- readr::read_tsv("data/pageview_counts_portal-ukwiki_20160629-20160828.tsv", col_types = "Dccccclllil")
pageviews <- readr::read_tsv("data/pageview_counts_portal-ukwiki_20160703-20160901.tsv", col_types = "Dcclccclllil")
pageviews <- dplyr::left_join(pageviews, polloi::get_prefixes()[, c("language", "prefix")], by = "prefix")
pageviews <- as.data.table(pageviews)

pageviews$post_deployment_indicator <- as.numeric(pageviews$post_deployment)
pageviews$site <- paste(tolower(pageviews$language), pageviews$project)
pageviews$page <- ifelse(pageviews$is_main_page, "(main page)", "(other pages)")
pageviews$from <- ifelse(pageviews$from_wikipedia_portal, "from Wikipedia.org Portal", "from elsewhere")
pageviews$from[pageviews$from_search_redirect] <- "from Wikipedia.org Portal (search-redirect.php)"
pageviews$from[pageviews$from_search_engine] <- "from a search engine"
pageviews$from[pageviews$referer_class == "none"] <- "direct (e.g. bookmark or homepage)"
pageviews$from[pageviews$referer_class == "internal" & !pageviews$from_wikipedia_portal] <- "from a Wikimedia project/tool/bot"
pageviews$source <- paste(pageviews$site, pageviews$page, pageviews$from)

interim <- pageviews[project == "wikipedia" & from_wikipedia_portal == FALSE & from_search_redirect == FALSE, ]
interim$source <- paste(interim$site, "not from Wikipedia.org Portal")
pageviews <- rbind(pageviews, interim); rm(interim)

# devtools::install_github("google/CausalImpact", force = TRUE)
# install.packages("dtw")
# devtools::install_github("klarsen1/MarketMatching", force = TRUE)
library(MarketMatching)

cat("Finding market matches...")
market_match <- pageviews[, list(pageviews = sum(pageviews)), by = c("date", "source")] %>%
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
cat("done.\n")
# dplyr::filter(market_match$BestMatches, source == "ukrainian wikipedia (main page) from Wikipedia.org Portal")[, c(2:6)]

best_markets <- dplyr::filter(market_match$BestMatches, source == "ukrainian wikipedia (main page) from Wikipedia.org Portal")$BestControl

n_iters <- 1e4
burn_in <- 1e3

make_data_bsts_ready <- function(data) {
  return({
    data %>%
    tidyr::spread(source, pageviews, fill = 0) %>%
    { xts::xts(.[, -1, with = FALSE], order.by = .$date) } %>%
    { .[, union("ukrainian wikipedia (main page) from Wikipedia.org Portal", names(.))] }
  })
}

make_prior <- function(data) {
  prior_inclusion <- rep(0, ncol(data))
  prior_inclusion[names(data) == "post_deployment"] <- 1
  prior_inclusion[names(data) %in% c(
    "russian wikipedia (main pages) from Wikipedia.org Portal",
    "ukrainian wikipedia (other pages) from Wikipedia.org Portal",
    "ukrainian wikipedia (main page) from a Wikimedia project/tool/bot")] <- 0.9
  # prior_inclusion[names(data) %in% c(
  #   "russian wikibooks (main pages) from elsewhere",
  #   "ukrainian wiktionary from a search engine")] <- 0.5
  prior_inclusion[names(data) == "ukrainian wikipedia (main page) not from Wikipedia.org Portal"] <- 0.6
  prior_inclusion[names(data) == "ukrainian wikipedia (other pages) not from Wikipedia.org Portal"] <- 0.1
  ## Unnecessary because of (Intercept) term:
  # prior_inclusion <- prior_inclusion[-which(names(data) == "ukrainian wikipedia (main page) from Wikipedia.org Portal")]
  # names(prior_inclusion) <- names(data)[-1]
  prior <- SpikeSlabPrior(x = model.matrix(`ukrainian wikipedia (main page) from Wikipedia.org Portal` ~ ., data = data),
                          y = data$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                          prior.inclusion.probabilities = prior_inclusion)
  return(prior)
}

model_1 <- function() {

  cat("Fitting model 'Matched markets, no AR/seasonality'\n")

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia (main page) from Wikipedia.org Portal", # response
                                    best_markets),
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  model <- bsts(`ukrainian wikipedia (main page) from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`),
                niter = burn_in + n_iters, seed = 0)
  return(model)

}

model_2 <- function() {

  cat("Fitting model 'Mixed markets, no AR/seasonality'\n")

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia (main page) from Wikipedia.org Portal", # response
                                    "russian wikipedia from Wikipedia.org Portal", # 1.7% incl. prob.
                                    "ukrainian wikipedia from a Wikimedia project/tool/bot",
                                    "russian wikibooks from elsewhere", # 6% incl. prob.
                                    "ukrainian wiktionary from a search engine"), # 3% incl. prob.
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`),
                niter = burn_in + n_iters, seed = 0)
  return(model)

}

model_3 <- function() {

  cat("Fitting model 'Matched markets, AR + seasonality'\n")

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia (main page) from Wikipedia.org Portal", # response
                                    best_markets),
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

}

model_4 <- function() {

  cat("Fitting model 'Mixed markets, AR + seasonality'\n")

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia (main page) from Wikipedia.org Portal", # response
                                    "russian wikipedia from Wikipedia.org Portal", # 1.7% incl. prob.
                                    "ukrainian wikipedia from a Wikimedia project/tool/bot",
                                    "russian wikibooks from elsewhere", # 6% incl. prob.
                                    "ukrainian wiktionary from a search engine"), # 3% incl. prob.
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

}

model_5 <- function() {

  cat("Fitting model 'Ruwiki from Portal, Ukwiki from Wikimedia, no AR/seasonality'\n")

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia (main page) from Wikipedia.org Portal", # response
                                    "russian wikipedia from Wikipedia.org Portal", # 1.7% incl. prob.
                                    "ukrainian wikipedia from a Wikimedia project/tool/bot"),
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

}

model_6 <- function() {

  cat("Fitting model 'Ruwiki from Portal, Ukwiki from Wikimedia, AR + seasonality'\n")

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia (main page) from Wikipedia.org Portal", # response
                                    "russian wikipedia from Wikipedia.org Portal", # 1.7% incl. prob.
                                    "ukrainian wikipedia from a Wikimedia project/tool/bot"),
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

}

model_7 <- function() {

  cat("Fitting model 'Ruwiki from Portal, AR + seasonality'\n")

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia (main page) from Wikipedia.org Portal", # response
                                    "russian wikipedia from Wikipedia.org Portal"),
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

}

model_8 <- function() {

  cat("Fitting model 'Ukwiki from elsewhere, AR + seasonality'\n")

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia (main page) from Wikipedia.org Portal", # response
                                    "ukrainian wikipedia from a Wikimedia project/tool/bot"),
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

}

model_9 <- function() {

  cat("Fitting model 'No markets, AR + seasonality'\n")

  ukwiki <- pageviews[source == "ukrainian wikipedia (main page) from Wikipedia.org Portal",
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

}

model_10 <- function() {

  cat("Fitting model 'No markets, seasonality, no AR'\n")

  ukwiki <- pageviews[source == "ukrainian wikipedia (main page) from Wikipedia.org Portal",
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

}

model_11 <- function() {

  cat("Fitting model 'No markets, AR, no seasonality'\n")

  ukwiki <- pageviews[source == "ukrainian wikipedia (main page) from Wikipedia.org Portal",
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`)
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

}

model_12 <- function() {

  cat("Fitting model 'No markets, no AR/seasonality'\n")

  ukwiki <- pageviews[source == "ukrainian wikipedia (main page) from Wikipedia.org Portal",
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`),
                niter = burn_in + n_iters, seed = 0)
  return(model)

}

model_13 <- function() {

  cat("Fitting model 'Ruwiki from Portal, Ukwiki not from Portal, AR + seasonality'\n")

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia (main page) from Wikipedia.org Portal", # response
                                    "russian wikipedia from Wikipedia.org Portal",
                                    "ukrainian wikipedia not from Wikipedia.org Portal"),
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

}

model_14 <- function() {

  cat("Fitting model 'Ukwiki not from Portal, AR + seasonality'\n")

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia (main page) from Wikipedia.org Portal", # response
                                    "ukrainian wikipedia not from Wikipedia.org Portal"),
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

}

model_15 <- function() {

  cat("Fitting model 'Ukwiki not from Portal, Rubooks from elsewhere, AR + seasonality'\n")

  ukwiki <- pageviews[source %in% c("ukrainian wikipedia (main page) from Wikipedia.org Portal", # response
                                    "russian wikibooks from elsewhere",
                                    "ukrainian wikipedia not from Wikipedia.org Portal"),
                      list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                      by = c("date", "source")] %>% make_data_bsts_ready

  ss <- AddLocalLinearTrend(list(), ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`)
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 7, season.duration = 1) # Weekly seasonality
  ss <- AddSeasonal(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`,
                    nseasons = 4, season.duration = 7) # Monthly seasonality
  ss <- AddAr(ss, ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`, lags = 1)

  model <- bsts(`ukrainian wikipedia from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS", prior = make_prior(ukwiki),
                state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  return(model)

}

model_list <- list(
  "Matched markets, no AR/seasonality" = model_1(),
  "Mixed markets, no AR/seasonality" = model_2(),
  "Matched markets, AR + seasonality" = model_3(),
  "Mixed markets, AR + seasonality" = model_4(),
  "Ruwiki from Portal, Ukwiki from Wikimedia, no AR/seasonality" = model_5(),
  "Ruwiki from Portal, Ukwiki from Wikimedia, AR + seasonality" = model_6(),
  "Ruwiki from Portal, AR + seasonality" = model_7(),
  "Ukwiki from Wikimedia, AR + seasonality" = model_8(),
  "No markets, AR + seasonality" = model_9(),
  "No markets, seasonality, no AR" = model_10(),
  "No markets, AR, no seasonality" = model_11(),
  "No markets, no AR/seasonality" = model_12(),
  "Ruwiki from Portal, Ukwiki not from Portal, AR + seasonality" = model_13(),
  "Ukwiki not from Portal, AR + seasonality" = model_14(),
  "Ukwiki not from Portal, Rubooks from elsewhere, AR + seasonality" = model_15()
)

# rm(model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8, model_9, model_10, model_11, model_12, model_13, model_14, model_15)

prediction_errors <- matrix(0, nrow = length(unique(pageviews$date)), ncol = length(model_list))
colnames(prediction_errors) <- names(model_list)
for (model_name in names(model_list)) {
  prediction_errors[, model_name] <- cumsum(abs(colMeans(bsts.prediction.errors(model_list[[model_name]])[-(1:burn_in), , drop = FALSE])))
}; rm(model_name)
prediction_errors <- as.data.frame(prediction_errors)
prediction_errors$Date <- unique(pageviews$date)

{
  prediction_errors %>%
  tidyr::gather(Model, `Cumulative Absolute Error`, -Date) %>%
  ggplot(aes(x = Date, y = `Cumulative Absolute Error`, color = Model)) +
  geom_line() +
  ggthemes::theme_tufte(12, "Gill Sans") +
  theme(legend.position = "bottom") +
  ggtitle("Cumulative absolute error over time by model",
          subtitle = "Bayesian structural time series (BSTS) models")
} %>% ggsave("cumulative_absolute_error.png", plot = ., path = figures_dir,
             width = 18, height = 6, dpi = 300)

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

for (model_name in names(model_list)) {
  cat("\n==============\n")
  print(which(names(model_list) == model_name))
  print(model_name)
  print(sprintf("R2: %.2f | GoF: %.2f | AIC: %.2f | BIC: %.2f",
                summary(model_list[[model_name]], burn = burn_in)$rsquare,
                summary(model_list[[model_name]], burn = burn_in)$relative.gof,
                AIC(model_list[[model_name]]),
                BIC(model_list[[model_name]])))
  print(summary(model_list[[model_name]], burn = burn_in)$coefficients["post_deployment", ])
}

quantiles <- matrix(0.0, nrow = length(model_list), ncol = 3)
rownames(quantiles) <- names(model_list)
for (model_name in names(model_list)) {
  quantiles[model_name, 1] <- summary(model_list[[model_name]], burn = burn_in)$rsquare
  quantiles[model_name, 2] <- AIC(model_list[[model_name]])
  quantiles[model_name, 3] <- BIC(model_list[[model_name]])
}; rm(model_name)
colnames(quantiles) <- c("R squared", "AIC", "BIC")
quantiles <- as.data.frame(quantiles)
knitr::kable(quantiles[order(quantiles$`R squared`, decreasing = TRUE), ],
             format = "markdown", digits = 3, align = c("r", "r", "r"))
knitr::kable(quantiles[order(quantiles$`AIC`, decreasing = FALSE), ],
             format = "markdown", digits = 3, align = c("r", "r", "r"))
knitr::kable(quantiles[order(quantiles$`BIC`, decreasing = FALSE), ],
             format = "markdown", digits = 3, align = c("r", "r", "r"))

cat("Saving BSTS stuff...")
save(list = c("market_match", "logLik.bsts", "model_list", "pageviews", "n_iters", "burn_in", "prediction_errors"),
     file = "data/bsts.RData")
cat("done.\n")

# model <- model_list[[6]]
# fit <- rowSums(aperm(model$state.contributions, c(1, 3, 2)), dims = 2)[-(1:burn_in), ]
# colnames(fit) <- as.character(zoo::index(model$original.series))
# fit <- as.data.frame(fit) %>%
#   cbind(iteration = 1:n_iters) %>%
#   tidyr::gather(date, value, -iteration) %>%
#   dplyr::mutate(date = as.Date(date)) %>%
#   dplyr::group_by(date) %>%
#   dplyr::summarize(point_estimate = median(value),
#                    lower_95 = quantile(value, 0.025),
#                    upper_95 = quantile(value, 0.975),
#                    lower_80 = quantile(value, 0.1),
#                    upper_80 = quantile(value, 0.9)) %>%
#   dplyr::left_join(data.frame(date = zoo::index(model$original.series),
#                               actual = as.numeric(model$original.series),
#                               stringsAsFactors = FALSE),
#                    by = "date") %>%
#   dplyr::mutate(deployment_effect_middle = median(model$coefficients[-(1:burn_in), "post_deployment"]) * as.numeric(date >= "2016-08-16"),
#                 deployment_effect_lower = quantile(model$coefficients[-(1:burn_in), "post_deployment"], 0.025) * as.numeric(date >= "2016-08-16"),
#                 deployment_effect_upper = quantile(model$coefficients[-(1:burn_in), "post_deployment"], 0.975) * as.numeric(date >= "2016-08-16"),
#                 sans = actual - deployment_effect_middle, sans_lower = actual - deployment_effect_lower, sans_upper = actual - deployment_effect_upper)
# MAPE <- fit %>%
#   # dplyr::filter(date < "2016-08-15") %>%
#   dplyr::summarize(mape = mean(abs(actual - point_estimate)/actual)) %>%
#   { .$mape }
# ggplot(fit, aes(x = date)) +
#   geom_ribbon(aes(ymin = lower_95, ymax = upper_95), alpha = 0.1, fill = "cornflowerblue") +
#   geom_ribbon(aes(ymin = lower_80, ymax = upper_80), alpha = 0.25, fill = "cornflowerblue") +
#   geom_line(aes(y = point_estimate), color = "cornflowerblue", size = 2) +
#   geom_ribbon(aes(ymin = sans_lower, ymax = sans_upper), alpha = 0.25, fill = "red") +
#   geom_line(aes(y = sans), color = "red", linetype = "dashed") +
#   geom_line(aes(y = actual)) + geom_point(aes(y = actual)) +
#   scale_y_continuous("Pageviews", labels = polloi::compress) +
#   scale_x_date("Date", date_breaks = "1 week", date_labels = "%d %b") +
#   geom_vline(xintercept = as.numeric(as.Date("2016-08-16")), linetype = "dashed") +
#   ggtitle(paste("Pageviews to", tolower(gsub("`", "", dimnames(attr(model$terms, "factors"))[[1]][1]))),
#           # subtitle = sprintf("Bayesian Structural Time Series model with mean absolute percentage error %.2f%%", 100 * MAPE),
#           subtitle = "Bayesian Structural Time Series model") +
#   ggthemes::theme_tufte(12, "Gill Sans")
# rm(model, fit)
