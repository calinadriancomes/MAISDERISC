# ============================================================
# R/06_predict.R  (fix select() mascat de MASS)
# ============================================================

predict_expected_counts <- function(model, newdata) {
  pred <- stats::predict(model, newdata = newdata, type = "response")
  newdata %>%
    dplyr::mutate(pred_TaxAuthorityLitigation = as.numeric(pred))
}

top_risk_table <- function(df_pred, n = 20) {
  df_pred %>%
    dplyr::arrange(dplyr::desc(pred_TaxAuthorityLitigation)) %>%
    dplyr::select(
      ID,
      Turnover,
      NetProfit,
      Employees,
      Litigation,
      TaxAuthorityLitigation,
      pred_TaxAuthorityLitigation
    ) %>%
    utils::head(n)
}