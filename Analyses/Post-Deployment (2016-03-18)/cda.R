library(xts)
library(forecast)

significance_mark <- function(p) {
  if (p < 0.001) {
    return("***")
  } else if (p < 0.01) {
    return("**")
  } else if (p < 0.05) {
    return("*")
  } else {
    return("")
  }
}

clickthrough_ts <- ts(clickthroughs$`naive clickthrough`, frequency = 1)
search_clickthrough_ts <- ts(search_clickthroughs$clickthrough, frequency = 1)
# as.numeric(factor(lubridate::floor_date(search_clickthroughs$date, "week")))

# fit <- auto.arima(search_clickthrough_ts[, 'clickthrough'],
#                   xreg = search_clickthrough_ts[, 'portal'],
#                   seasonal = TRUE, allowmean = FALSE, ic = "bic",
#                   stepwise = FALSE, parallel = TRUE, num.cores = 2)
new_portal <- as.numeric(search_clickthroughs$portal == "new")

fit <- arima(search_clickthrough_ts,
             order = c(2, 1, 2),
             seasonal = list(order = c(1, 1, 1), period = 7),
             xreg = new_portal)
c(AIC = AIC(fit), BIC = BIC(fit))

par(mfrow = c(2, 1))
plot(search_clickthrough_ts, lwd = 2,
     ylab = "Rate", xlab = "Day",
     main = "Search Clickthrough Rate on Wikipedia Portal",
     yaxt = "n", ylim = c(0.35, 0.5), xaxt = "n")
axis(2, at = seq(0.35, 0.5, 0.05), labels = sprintf("%.0f%%", seq(35, 50, 5)), las = 2)
axis(1, at = seq(1, nrow(search_clickthroughs), 7),
     labels = as.character(search_clickthroughs$date[seq(1, nrow(search_clickthroughs), 7)], format = "%a (%d %b)"))
abline(v = which(search_clickthroughs$date == lubridate::ymd("2016-03-10")), lty = "dashed", col = "blue", lwd = 2)
lines(residuals(fit) + search_clickthrough_ts, col = "red", lty = "dotted", lwd = 2)
legend("topleft", c("ARIMA(2,1,2)(1,1,1)7 Fit", "New Portal Deployed"),
       lty = c("dotted", "dashed"), col = c("red", "blue"), lwd = 2, bty = "n")
acf(residuals(fit), lwd = 2, main = "Sample Autocorrelation Function of Residuals")

fit %>%
  accuracy %>%
  broom::tidy() %>%
  dplyr::select(-.rownames) %>%
  knitr::kable(digits = 4)

fit %>%
  broom::tidy() %>%
  mutate(p.value = (1-pnorm(abs(fit$coef)/sqrt(diag(fit$var.coef))))*2) %>%
  filter(term == "new_portal") %>%
  mutate(lower = estimate - 1.96 * std.error,
         upper = estimate + 1.96 * std.error) %>%
  transmute(Estimate = estimate,
            `Std. Error` = std.error,
            `95% C.I.` = sprintf("(%.4f, %.4f)", lower, upper),
            `p-value` = p.value,
            ` ` = significance_mark(`p-value`)) %>%
  knitr::kable(digits = 4, align = c("r", "r", "c"))


