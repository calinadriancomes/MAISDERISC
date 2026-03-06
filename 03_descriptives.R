# R/03_descriptives.R
summary_counts <- function(df) {
  tibble::tibble(
    n = nrow(df),
    y_mean = mean(df$TaxAuthorityLitigation, na.rm = TRUE),
    y_median = median(df$TaxAuthorityLitigation, na.rm = TRUE),
    y_var = var(df$TaxAuthorityLitigation, na.rm = TRUE),
    y_min = min(df$TaxAuthorityLitigation, na.rm = TRUE),
    y_max = max(df$TaxAuthorityLitigation, na.rm = TRUE)
  ) %>%
    mutate(overdispersion_hint = y_var / pmax(y_mean, 1e-9))
}

plot_y_distribution <- function(df) {
  ggplot(df, aes(x = TaxAuthorityLitigation)) +
    geom_histogram(bins = 30) +
    labs(title = "Distribuția litigiilor cu ANAF", x = "TaxAuthorityLitigation", y = "Frecvență") +
    theme_minimal(base_size = 12)
}

plot_scatter_log <- function(df, xvar = "Turnover") {
  ggplot(df, aes(x = .data[[xvar]], y = TaxAuthorityLitigation)) +
    geom_point(alpha = 0.25) +
    scale_x_continuous(trans = "log1p") +
    labs(title = paste0("Relația dintre ", xvar, " și litigii cu ANAF"), x = xvar, y = "TaxAuthorityLitigation") +
    theme_minimal(base_size = 12)
}