# ============================================================
# R/04_models.R  (DA CAPO AL FINE – select() fix + robust NB fit)
# ============================================================

build_formula <- function(df, use_nontax = TRUE, nonlinear = FALSE, y_name = "TaxAuthorityLitigation") {
  base_terms <- c("LogTurnover", "LogEmployees", "LossFlag", "LogAbsNetProfit")
  lit_term <- if (use_nontax) "NonTaxLitigation" else "Litigation"
  
  if (nonlinear) {
    ok_turn <- dplyr::n_distinct(df$LogTurnover, na.rm = TRUE) >= 10
    ok_emp  <- dplyr::n_distinct(df$LogEmployees, na.rm = TRUE) >= 10
    
    if (ok_turn && ok_emp) {
      return(stats::as.formula(
        paste(
          y_name, "~",
          "splines::ns(LogTurnover, 4) +",
          "splines::ns(LogEmployees, 4) +",
          "LossFlag + LogAbsNetProfit +",
          lit_term
        )
      ))
    }
  }
  
  stats::as.formula(paste(y_name, "~", paste(c(base_terms, lit_term), collapse = " + ")))
}

fit_poisson <- function(df, formula) {
  stats::glm(formula, data = df, family = poisson(link = "log"))
}

fit_negbin <- function(df, formula) {
  MASS::glm.nb(formula, data = df, control = glm.control(maxit = 100))
}

robust_fit_negbin <- function(df, use_nontax, nonlinear) {
  # 1) încercare: y original + (eventual) splines
  f1 <- build_formula(df, use_nontax = use_nontax, nonlinear = nonlinear, y_name = "TaxAuthorityLitigation")
  m1 <- tryCatch(fit_negbin(df, f1), error = function(e) e)
  
  if (!inherits(m1, "error")) {
    return(list(model = m1, used = "NegBin (y original)", formula = f1))
  }
  
  # 2) fallback: fără splines (mai stabil)
  f2 <- build_formula(df, use_nontax = use_nontax, nonlinear = FALSE, y_name = "TaxAuthorityLitigation")
  m2 <- tryCatch(fit_negbin(df, f2), error = function(e) e)
  
  if (!inherits(m2, "error")) {
    return(list(model = m2, used = "NegBin (fără splines)", formula = f2))
  }
  
  # 3) fallback: winsorizare y pentru fitting (p99), apoi NB fără splines
  dfw <- add_winsor_y(df, p = 0.99)
  f3 <- build_formula(dfw, use_nontax = use_nontax, nonlinear = FALSE, y_name = "TaxAuthorityLitigation_w")
  m3 <- tryCatch(fit_negbin(dfw, f3), error = function(e) e)
  
  if (!inherits(m3, "error")) {
    return(list(model = m3, used = "NegBin (y winsorizat p99)", formula = f3))
  }
  
  # 4) ultim fallback: Poisson (ca să nu crape aplicația)
  m4 <- fit_poisson(df, f2)
  list(model = m4, used = "Poisson fallback", formula = f2)
}

compare_models_aic <- function(m_pois, m_nb) {
  data.frame(
    model = c("Poisson", "Model robust"),
    AIC = c(AIC(m_pois), AIC(m_nb)),
    BIC = c(BIC(m_pois), BIC(m_nb)),
    row.names = NULL
  )
}

tidy_rate_ratios <- function(model) {
  out <- broom::tidy(model, conf.int = TRUE) %>%
    dplyr::mutate(
      rate_ratio = exp(estimate),
      conf.low.rr = exp(conf.low),
      conf.high.rr = exp(conf.high)
    ) %>%
    dplyr::select(term, estimate, std.error, statistic, p.value, rate_ratio, conf.low.rr, conf.high.rr)
  
  as.data.frame(out)
}