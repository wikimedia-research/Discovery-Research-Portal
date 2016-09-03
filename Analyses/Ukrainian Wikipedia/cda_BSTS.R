source("refine.R")

library(bsts)
library(ggplot2)
library(dygraphs)

interim <- pageviews[project == "wikipedia" & from_wikipedia_portal == FALSE & from_search_redirect == FALSE, ]
interim$source <- paste(interim$site, "not from Wikipedia.org Portal")
pageviews <- rbind(pageviews, interim); rm(interim)

interim <- pageviews[source == "ukrainian wikipedia (main page) from Wikipedia.org Portal" &
                       speaks_ukrainian == FALSE &
                       speaks_russian == TRUE, ]
interim$source <- "Ukrainian Wikipedia (Main Page) PVs from Portal's Russian-but-not-Ukrainian-speaking visitors"
pageviews <- rbind(pageviews, interim); rm(interim)

pageviews$valid <- TRUE
pageviews$valid[pageviews$project != "wikipedia" & (pageviews$from_wikipedia_portal == TRUE | pageviews$from_search_redirect == TRUE)] <- FALSE
pageviews$valid[pageviews$project == "wikipedia" & pageviews$from_wikipedia_portal == TRUE & pageviews$page != "(main page)"] <- FALSE

invalid <- pageviews[valid == TRUE, list(pageviews = sum(pageviews)), by = c("date", "source")] %>%
  tidyr::spread(source, pageviews, fill = 0) %>%
  tidyr::gather(source, pageviews, -date) %>%
  dplyr::group_by(source) %>%
  dplyr::summarize(mpv = median(pageviews)) %>%
  dplyr::filter(mpv < 20) %>%
  { .$source }
pageviews$valid[pageviews$source %in% invalid] <- FALSE; rm(invalid)

# devtools::install_github("google/CausalImpact", force = TRUE)
# install.packages("dtw")
# devtools::install_github("klarsen1/MarketMatching", force = TRUE)
library(MarketMatching)
cat("Finding market matches...")
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
cat("done.\n")
# View(dplyr::filter(market_match$BestMatches, source == "Ukrainian Wikipedia (Main Page) PVs from Portal's Russian-but-not-Ukrainian-speaking visitors")[, c(2:6)])

source("bsts_funcs.R")
n_iters <- 1e4
burn_in <- 1e3
best_markets <- dplyr::filter(market_match$BestMatches, source == "Ukrainian Wikipedia (Main Page) PVs from Portal's Russian-but-not-Ukrainian-speaking visitors")$BestControl

model_list <- list(

  "RuBNUkS // No control markets" = make_model(pageviews),

  "RuBNUkS // No control markets, AR, or seasonalities" = make_model(pageviews,
    seasonality = c("weekly" = FALSE, "monthly" = FALSE),
    autoregressive = FALSE),

  "RuBNUkS // Matched markets only" = make_model(pageviews, best_markets,
    seasonality = c("weekly" = FALSE, "monthly" = FALSE),
    autoregressive = FALSE),

  "RuBNUkS // Matched markets with AR(1) & seasonalities" = make_model(pageviews, best_markets),

  "RuBNUkS // Selected markets" = make_model(pageviews, c(
    "russian wikipedia (main page) from Wikipedia.org Portal",
    "german wikipedia (main page) from Wikipedia.org Portal",
    "ukrainian wikipedia (other pages) from Wikipedia.org Portal",
    "ukrainian wikipedia (main page) not from Wikipedia.org Portal",
    "ukrainian wikipedia (main page) from a Wikimedia project/tool/bot"
  )),

  "RuBNUkS // Mix of matched & selected markets" = make_model(pageviews, union(c(
    "russian wikipedia (main page) from Wikipedia.org Portal",
    "german wikipedia (main page) from Wikipedia.org Portal",
    "ukrainian wikipedia (other pages) from Wikipedia.org Portal",
    "ukrainian wikipedia (main page) not from Wikipedia.org Portal",
    "ukrainian wikipedia (main page) from a Wikimedia project/tool/bot"),
    head(best_markets, 2))),

  "RuBNUkS // Ruwiki Main from Portal & Ukwiki Other from Portal" = make_model(pageviews, c(
    "russian wikipedia (main page) from Wikipedia.org Portal",
    "ukrainian wikipedia (other pages) from Wikipedia.org Portal"
  )),

  "RuBNUkS // Ruwiki Main from Portal" = make_model(pageviews, "russian wikipedia (main page) from Wikipedia.org Portal"),

  "RuBNUkS // Ukwiki Other from Portal" = make_model(pageviews, "ukrainian wikipedia (other pages) from Wikipedia.org Portal"),

  "RuBNUks in Ukraine // No control markets" = make_model(pageviews[country == "Ukraine", ]),

  "RuBNUks in Ukraine // Ukraine's Ukwiki Other from Portal" = make_model(pageviews[country == "Ukraine", ], "ukrainian wikipedia (other pages) from Wikipedia.org Portal"),

  "RuBNUks in Ukraine // Ukraine's Ruwiki Main from Portal" = make_model(pageviews[country == "Ukraine", ], "russian wikipedia (main page) from Wikipedia.org Portal"),

  "All Speakers // No control markets" = make_model_2(pageviews),

  "All Speakers // Ruwiki Main from Portal & Ukwiki Other from Portal" = make_model_2(pageviews, c(
    "russian wikipedia (main page) from Wikipedia.org Portal",
    "ukrainian wikipedia (other pages) from Wikipedia.org Portal"
  )),

  "All Speakers // Ruwiki Main from Portal" = make_model_2(pageviews, "russian wikipedia (main page) from Wikipedia.org Portal"),

  "All Speakers // Ukwiki Other from Portal" = make_model_2(pageviews, "ukrainian wikipedia (other pages) from Wikipedia.org Portal"),

  "All Speakers in Ukraine // No control markets" = make_model_2(pageviews[country == "Ukraine", ]),

  "All Speakers in Ukraine // Ruwiki Main from Portal & Ukwiki Other from Portal" = make_model_2(pageviews[country == "Ukraine", ], c(
    "russian wikipedia (main page) from Wikipedia.org Portal",
    "ukrainian wikipedia (other pages) from Wikipedia.org Portal"
  )),

  "All Speakers in Ukraine // Ruwiki Main from Portal" = make_model_2(pageviews[country == "Ukraine", ], "russian wikipedia (main page) from Wikipedia.org Portal"),

  "All Speakers in Ukraine // Ukwiki Other from Portal" = make_model_2(pageviews[country == "Ukraine", ], "ukrainian wikipedia (other pages) from Wikipedia.org Portal")

)

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
  dplyr::mutate(Population = sub("(.*) // (.*)", "\\1", Model),
                Model = sub("(.*) // (.*)", "\\2", Model)) %>%
  ggplot(aes(x = Date, y = `Cumulative Absolute Error`, color = Model)) +
  geom_line() +
  facet_wrap(~ Population, ncol = 1, scales = "free_y") +
  ggthemes::theme_tufte(12, "Gill Sans") +
  theme(legend.position = "bottom") +
  ggtitle("Cumulative absolute error over time by model and population",
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

# for (model_name in names(model_list)) {
#   cat("\n==============\n")
#   print(which(names(model_list) == model_name))
#   print(model_name)
#   print(sprintf("R2: %.2f | GoF: %.2f | AIC: %.2f | BIC: %.2f",
#                 summary(model_list[[model_name]], burn = burn_in)$rsquare,
#                 summary(model_list[[model_name]], burn = burn_in)$relative.gof,
#                 AIC(model_list[[model_name]]),
#                 BIC(model_list[[model_name]])))
#   print(summary(model_list[[model_name]], burn = burn_in)$coefficients["post_deployment", ])
# }

quantiles <- matrix(0.0, nrow = length(model_list), ncol = 6)
rownames(quantiles) <- names(model_list)
for (model_name in names(model_list)) {
  quantiles[model_name, 1] <- summary(model_list[[model_name]], burn = burn_in)$rsquare
  quantiles[model_name, 2] <- AIC(model_list[[model_name]])
  quantiles[model_name, 3] <- BIC(model_list[[model_name]])
  quantiles[model_name, 4:6] <- quantile(model_list[[model_name]]$coefficients[-(1:burn_in), "post_deployment"], c(0.5, 0.025, 0.975))
}; rm(model_name)
colnames(quantiles) <- c("R squared", "AIC", "BIC", "Point Estimate", "Lower Bound", "Upper Bound")
quantiles <- as.data.frame(quantiles)
View(quantiles)
# knitr::kable(quantiles[order(quantiles$`R squared`, decreasing = TRUE), ],
#              format = "markdown", digits = 3, align = c("r", "r", "r"))
# knitr::kable(quantiles[order(quantiles$`AIC`, decreasing = FALSE), ],
#              format = "markdown", digits = 3, align = c("r", "r", "r"))
# knitr::kable(quantiles[order(quantiles$`BIC`, decreasing = FALSE), ],
#              format = "markdown", digits = 3, align = c("r", "r", "r"))

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
