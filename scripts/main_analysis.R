packages <- c("quantmod", "PerformanceAnalytics", "ggplot2")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)

dir.create("data", showWarnings = FALSE)

end_date <- Sys.Date()
start_date <- end_date - 365 * 3

aapl <- getSymbols(
  "AAPL", src = "yahoo",
  from = start_date, to = end_date,
  auto.assign = FALSE
)

sp500 <- getSymbols(
  "^GSPC", src = "yahoo",
  from = start_date, to = end_date,
  auto.assign = FALSE
)

prices <- merge(
  AAPL = Ad(aapl),
  SP500 = Ad(sp500),
  join = "inner"
)

prices <- na.omit(prices)
returns <- na.omit(CalculateReturns(prices, method = "log"))

risk_one <- function(x, asset) {
  data.frame(
    Asset = asset,
    Historical_VaR_95 = as.numeric(VaR(x, p = 0.95, method = "historical")),
    Historical_VaR_99 = as.numeric(VaR(x, p = 0.99, method = "historical")),
    Parametric_VaR_95 = as.numeric(VaR(x, p = 0.95, method = "gaussian")),
    Parametric_VaR_99 = as.numeric(VaR(x, p = 0.99, method = "gaussian")),
    Historical_ES_95 = as.numeric(ES(x, p = 0.95, method = "historical"))
  )
}

results <- rbind(
  risk_one(returns[, "AAPL"], "Apple (AAPL)"),
  risk_one(returns[, "SP500"], "S&P 500")
)

results[, -1] <- results[, -1] * 100

print(round(results, 3))

write.csv(results, "data/risk_metrics.csv", row.names = FALSE)
write.csv(
  data.frame(Date = index(returns), coredata(returns)),
  "data/log_returns.csv",
  row.names = FALSE
)

plot_data <- data.frame(
  Asset = rep(colnames(returns), each = nrow(returns)),
  Return = as.vector(coredata(returns))
)

var_data <- do.call(rbind, lapply(seq_len(ncol(returns)), function(i) {
  x <- coredata(returns[, i])
  data.frame(
    Asset = colnames(returns)[i],
    Confidence = c("VaR 95%", "VaR 99%"),
    Threshold = as.numeric(quantile(x, c(0.05, 0.01)))
  )
}))

chart <- ggplot(plot_data, aes(x = Return)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "steelblue", color = "white") +
  geom_vline(
    data = var_data,
    aes(xintercept = Threshold, linetype = Confidence),
    color = "red"
  ) +
  facet_wrap(~Asset, scales = "free") +
  labs(
    title = "Distribution of Daily Log Returns",
    x = "Log return",
    y = "Density",
    linetype = "Historical VaR"
  ) +
  theme_minimal()

print(chart)

ggsave("data/var_chart.png", chart, width = 10, height = 6, dpi = 300)

write.csv(
  data.frame(Date = index(prices), coredata(prices)),
  "data/adjusted_prices.csv",
  row.names = FALSE
)

cat("Analysis completed successfully.\n")
