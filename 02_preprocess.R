# ============================================================
# R/02_preprocess.R   – stabilizare prin standardizare
# ============================================================
preprocess_for_count_models <- function(df) {
  df <- df %>%
    dplyr::mutate(
      TaxAuthorityLitigation = ifelse(is.na(TaxAuthorityLitigation), 0, TaxAuthorityLitigation),
      Litigation = ifelse(is.na(Litigation), 0, Litigation),
      Employees = ifelse(is.na(Employees), 0, Employees),
      Turnover = ifelse(is.na(Turnover), 0, Turnover),
      NetProfit = ifelse(is.na(NetProfit), 0, NetProfit),
      LossFlag = as.integer(NetProfit < 0),
      LogTurnover = log1p(pmax(Turnover, 0)),
      LogEmployees = log1p(pmax(Employees, 0)),
      LogAbsNetProfit = log1p(abs(NetProfit)),
      NonTaxLitigation = pmax(Litigation - TaxAuthorityLitigation, 0)
    )
  
  # standardizare pentru stabilitate numerică în exp()
  df <- df %>%
    dplyr::mutate(
      LogTurnover_z  = as.numeric(scale(LogTurnover)),
      LogEmployees_z = as.numeric(scale(LogEmployees))
    )
  df
}

filter_analysis_sample <- function(df, min_tax = 0, min_total = 0) {
  df %>%
    dplyr::filter(
      !is.na(TaxAuthorityLitigation) & TaxAuthorityLitigation >= min_tax,
      !is.na(Litigation) & Litigation >= min_total
    )
}

clean_for_model <- function(df) {
  df %>%
    dplyr::mutate(
      TaxAuthorityLitigation = ifelse(is.finite(TaxAuthorityLitigation), TaxAuthorityLitigation, NA_real_),
      Litigation = ifelse(is.finite(Litigation), Litigation, NA_real_),
      Turnover = ifelse(is.finite(Turnover), Turnover, NA_real_),
      NetProfit = ifelse(is.finite(NetProfit), NetProfit, NA_real_),
      Employees = ifelse(is.finite(Employees), Employees, NA_real_),
      LogTurnover = ifelse(is.finite(LogTurnover), LogTurnover, NA_real_),
      LogEmployees = ifelse(is.finite(LogEmployees), LogEmployees, NA_real_),
      LogAbsNetProfit = ifelse(is.finite(LogAbsNetProfit), LogAbsNetProfit, NA_real_),
      NonTaxLitigation = ifelse(is.finite(NonTaxLitigation), NonTaxLitigation, NA_real_),
      LogTurnover_z = ifelse(is.finite(LogTurnover_z), LogTurnover_z, NA_real_),
      LogEmployees_z = ifelse(is.finite(LogEmployees_z), LogEmployees_z, NA_real_),
      LossFlag = ifelse(is.na(LossFlag), 0L, LossFlag)
    ) %>%
    dplyr::filter(
      !is.na(TaxAuthorityLitigation),
      !is.na(LogTurnover_z),
      !is.na(LogEmployees_z),
      !is.na(LogAbsNetProfit),
      !is.na(NonTaxLitigation),
      !is.na(LossFlag)
    )
}
add_winsor_y <- function(df, p = 0.99) {
  cap <- as.numeric(stats::quantile(df$TaxAuthorityLitigation, probs = p, na.rm = TRUE))
  df %>% dplyr::mutate(TaxAuthorityLitigation_w = pmin(TaxAuthorityLitigation, cap))
}