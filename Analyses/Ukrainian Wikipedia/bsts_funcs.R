make_data_bsts_ready <- function(data, response) {
  return({
    data %>%
      tidyr::spread(source, pageviews, fill = 0) %>%
      { xts::xts(.[, -1, with = FALSE], order.by = .$date) } %>%
      { .[, union(response, names(.))] }
  })
}

make_model <- function(data,
                       control_markets = NULL,
                       seasonality = c(weekly = TRUE, monthly = TRUE),
                       autoregressive = TRUE,
                       ar_lags = ifelse(autoregressive, 1, 0),
                       model_name = NULL) {
  if (!is.null(model_name)) {
    cat(paste0("Fitting model '", model_name, "'..."))
  }
  # Aggegated Data:
  ukwiki <- data[source %in% c("Ukrainian Wikipedia (Main Page) PVs from Portal's Russian-but-not-Ukrainian-speaking visitors", control_markets),
                 list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                 by = c("date", "source")] %>% make_data_bsts_ready("Ukrainian Wikipedia (Main Page) PVs from Portal's Russian-but-not-Ukrainian-speaking visitors")
  y <- ukwiki$`Ukrainian Wikipedia (Main Page) PVs from Portal's Russian-but-not-Ukrainian-speaking visitors`
  # State Specification:
  ss <- AddLocalLinearTrend(list(), y)
  if (seasonality['weekly']) {
    ss <- AddSeasonal(ss, y, nseasons = 7, season.duration = 1) # Weekly seasonality
  }
  if (seasonality['monthly']) {
    ss <- AddSeasonal(ss, y, nseasons = 4, season.duration = 7) # Monthly seasonality
  }
  if (autoregressive) {
    ss <- AddAr(ss, y, lags = ar_lags)
  }
  # Prior:
  prior_inclusion <- rep(0, ncol(ukwiki))
  prior_inclusion[names(ukwiki) == "post_deployment"] <- 1
  prior_inclusion[names(ukwiki) %in% c(
    "russian wikipedia (main pages) from Wikipedia.org Portal",
    "ukrainian wikipedia (other pages) from Wikipedia.org Portal",
    "ukrainian wikipedia (main page) from a Wikimedia project/tool/bot")] <- 0.9
  prior_inclusion[names(ukwiki) == "ukrainian wikipedia (main page) not from Wikipedia.org Portal"] <- 0.6
  prior_inclusion[names(ukwiki) %in% c(
    "ukrainian wikivoyage (other pages) from elsewhere",
    "ukrainian wiktionary (main page) from a Wikimedia project/tool/bot")] <- 0.1
  prior_inclusion[names(ukwiki) == "ukrainian wikipedia (other pages) not from Wikipedia.org Portal"] <- 0.05
  ss_prior <- SpikeSlabPrior(x = model.matrix(`Ukrainian Wikipedia (Main Page) PVs from Portal's Russian-but-not-Ukrainian-speaking visitors` ~ ., data = ukwiki),
                             y = y, prior.inclusion.probabilities = prior_inclusion)
  # BSTS Model:
  model <- bsts(`Ukrainian Wikipedia (Main Page) PVs from Portal's Russian-but-not-Ukrainian-speaking visitors` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS",
                prior = ss_prior, state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  # Output:
  return(model)
}

make_model_2 <- function(data,
                         control_markets = NULL,
                         seasonality = c(weekly = TRUE, monthly = TRUE),
                         autoregressive = TRUE,
                         ar_lags = ifelse(autoregressive, 1, 0),
                         model_name = NULL) {
  if (!is.null(model_name)) {
    cat(paste0("Fitting model '", model_name, "'..."))
  }
  # Aggegated Data:
  ukwiki <- data[source %in% c("ukrainian wikipedia (main page) from Wikipedia.org Portal", control_markets),
                 list(pageviews = sum(pageviews), post_deployment = max(post_deployment_indicator)),
                 by = c("date", "source")] %>% make_data_bsts_ready("ukrainian wikipedia (main page) from Wikipedia.org Portal")
  y <- ukwiki$`ukrainian wikipedia (main page) from Wikipedia.org Portal`
  # State Specification:
  ss <- AddLocalLinearTrend(list(), y)
  if (seasonality['weekly']) {
    ss <- AddSeasonal(ss, y, nseasons = 7, season.duration = 1) # Weekly seasonality
  }
  if (seasonality['monthly']) {
    ss <- AddSeasonal(ss, y, nseasons = 4, season.duration = 7) # Monthly seasonality
  }
  if (autoregressive & ar_lags > 0) {
    ss <- AddAr(ss, y, lags = ar_lags)
  }
  # Prior:
  prior_inclusion <- rep(0, ncol(ukwiki))
  prior_inclusion[names(ukwiki) == "post_deployment"] <- 1
  prior_inclusion[names(ukwiki) %in% c(
    "russian wikipedia (main pages) from Wikipedia.org Portal",
    "ukrainian wikipedia (other pages) from Wikipedia.org Portal",
    "ukrainian wikipedia (main page) from a Wikimedia project/tool/bot")] <- 0.9
  prior_inclusion[names(ukwiki) == "ukrainian wikipedia (main page) not from Wikipedia.org Portal"] <- 0.6
  prior_inclusion[names(ukwiki) %in% c(
    "ukrainian wikivoyage (other pages) from elsewhere",
    "ukrainian wiktionary (main page) from a Wikimedia project/tool/bot")] <- 0.1
  prior_inclusion[names(ukwiki) == "ukrainian wikipedia (other pages) not from Wikipedia.org Portal"] <- 0.05
  ss_prior <- SpikeSlabPrior(x = model.matrix(`ukrainian wikipedia (main page) from Wikipedia.org Portal` ~ ., data = ukwiki),
                             y = y, prior.inclusion.probabilities = prior_inclusion)
  # BSTS Model:
  model <- bsts(`ukrainian wikipedia (main page) from Wikipedia.org Portal` ~ ., data = ukwiki,
                family = "gaussian", bma.method = "SSVS",
                prior = ss_prior, state.specification = ss,
                niter = burn_in + n_iters, seed = 0)
  # Output:
  return(model)
}
