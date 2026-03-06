# =========================================================================
# R/08_dashboard_helpers.R  (funcții pentru noul tab - Tablou de bord)
#
# gauge - manometru (0 la stânga, 100 la dreapta)
# =========================================================================


risk_score_0_100 <- function(df_pred, p = 0.99) {
  x <- df_pred$pred_TaxAuthorityLitigation
  cap <- as.numeric(stats::quantile(x, probs = p, na.rm = TRUE))
  xw <- pmin(x, cap)
  lo <- min(xw, na.rm = TRUE)
  hi <- max(xw, na.rm = TRUE)
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) return(rep(50, nrow(df_pred)))
  100 * (xw - lo) / (hi - lo)
}

risk_score_one <- function(all_scores, id_vector, target_id) {
  i <- which(id_vector == target_id)[1]
  if (is.na(i)) return(NA_real_)
  as.numeric(all_scores[i])
}

label_risk_band <- function(score) {
  ifelse(score < 33, "Scăzut", ifelse(score < 66, "Moderat", "Ridicat"))
}

make_gauge_plot <- function(score) {
  score <- max(0, min(100, as.numeric(score)))
  
  # 0 la stânga, 100 la dreapta
  theta <- pi * (score / 100)
  
  cx <- 0.5
  cy <- 0.10
  r  <- 0.38
  
  needle_x <- cx - r * cos(theta)
  needle_y <- cy + r * sin(theta)
  
  ggplot2::ggplot() +
    ggplot2::annotate("segment", x = 0.12, xend = 0.32, y = cy, yend = cy, size = 6, alpha = 0.6) +
    ggplot2::annotate("segment", x = 0.34, xend = 0.66, y = cy, yend = cy, size = 6, alpha = 0.35) +
    ggplot2::annotate("segment", x = 0.68, xend = 0.88, y = cy, yend = cy, size = 6, alpha = 0.6) +
    ggplot2::annotate("point", x = cx, y = cy, size = 6) +
    ggplot2::annotate("segment", x = cx, y = cy, xend = needle_x, yend = needle_y, linewidth = 1.2) +
    ggplot2::annotate("text", x = 0.5, y = 0.55,
                      label = paste0("Scor risc: ", round(score), "/100"),
                      size = 5) +
    ggplot2::annotate("text", x = 0.5, y = 0.42,
                      label = paste0("Nivel: ", label_risk_band(score)),
                      size = 4) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 0.7)) +
    ggplot2::theme_void()
}

elasticities_economic_text <- function(rr_df) {
  rr_df <- rr_df %>% dplyr::filter(term != "(Intercept)") %>% dplyr::mutate(rr = rate_ratio)
  
  pick <- function(term_key, label) {
    row <- rr_df %>% dplyr::filter(term == term_key) %>% dplyr::slice(1)
    if (nrow(row) == 0) return(NA_character_)
    rr <- row$rr[1]
    pct <- (rr - 1) * 100
    paste0(label, " are un efect multiplicativ estimat de ", round(rr, 3),
           ", ceea ce corespunde aproximativ unei modificări de ", round(pct, 1),
           "% a numărului așteptat de litigii cu ANAF, la o creștere cu o unitate a predictorului (în scala definită în model).")
  }
  
  txt1 <- pick("LogTurnover_z", "Cifra de afaceri (LogTurnover_z)")
  txt2 <- pick("LogEmployees_z", "Dimensiunea firmei (LogEmployees_z)")
  txt3 <- pick("LossFlag", "Faptul că firma este pe pierdere (LossFlag)")
  txt4 <- pick("NonTaxLitigation", "Litigiile non-ANAF (NonTaxLitigation)")
  txt5 <- pick("Litigation", "Totalul litigiilor (Litigation)")
  
  paste(na.omit(c(txt1, txt2, txt3, txt4, txt5)), collapse = " ")
}

fit_nb_original_and_winsor <- function(df, use_nontax, nonlinear) {
  f_orig <- build_formula(df, use_nontax = use_nontax, nonlinear = nonlinear, y_name = "TaxAuthorityLitigation")
  m_orig <- tryCatch(fit_negbin(df, f_orig), error = function(e) e)
  
  dfw <- add_winsor_y(df, p = 0.99)
  f_win <- build_formula(dfw, use_nontax = use_nontax, nonlinear = FALSE, y_name = "TaxAuthorityLitigation_w")
  m_win <- tryCatch(fit_negbin(dfw, f_win), error = function(e) e)
  
  list(m_orig = m_orig, f_orig = f_orig, m_win = m_win, f_win = f_win)
}

sensitivity_table <- function(m_orig, m_win) {
  aic_orig <- if (inherits(m_orig, "error")) NA_real_ else AIC(m_orig)
  aic_win  <- if (inherits(m_win, "error"))  NA_real_ else AIC(m_win)
  
  data.frame(
    Model = c("NegBin pe y original", "NegBin pe y winsorizat (p99)"),
    AIC = c(aic_orig, aic_win),
    Status = c(ifelse(is.na(aic_orig), "Eșuat", "OK"), ifelse(is.na(aic_win), "Eșuat", "OK")),
    row.names = NULL
  )
}

partial_effect_logturnover <- function(model, df_ref, grid_n = 80) {
  rng <- range(df_ref$LogTurnover_z, na.rm = TRUE)
  grid <- data.frame(LogTurnover_z = seq(rng[1], rng[2], length.out = grid_n))
  
  ref <- df_ref %>%
    dplyr::summarise(
      LogEmployees_z = stats::median(LogEmployees_z, na.rm = TRUE),
      LossFlag = stats::median(LossFlag, na.rm = TRUE),
      LogAbsNetProfit = stats::median(LogAbsNetProfit, na.rm = TRUE),
      NonTaxLitigation = stats::median(NonTaxLitigation, na.rm = TRUE),
      Litigation = stats::median(Litigation, na.rm = TRUE)
    )
  
  newd <- grid %>%
    dplyr::mutate(
      LogEmployees_z = ref$LogEmployees_z,
      LossFlag = ref$LossFlag,
      LogAbsNetProfit = ref$LogAbsNetProfit,
      NonTaxLitigation = ref$NonTaxLitigation,
      Litigation = ref$Litigation
    )
  
  mu <- stats::predict(model, newdata = newd, type = "response")
  newd %>% dplyr::mutate(mu = as.numeric(mu))
}

plot_partial_effect <- function(df_pe) {
  ggplot2::ggplot(df_pe, ggplot2::aes(x = LogTurnover_z, y = mu)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title = "Curbă parțială: efectul LogTurnover_z asupra litigiilor ANAF",
      x = "LogTurnover_z (standardizat)",
      y = "Număr așteptat litigii ANAF (μ)"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}