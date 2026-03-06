# R/01_io.R

read_input_data <- function(path) {
  df <- readxl::read_excel(path)
  names(df) <- stringr::str_squish(names(df))
  df
}

.standardize_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\s", "")
  x <- stringr::str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

standardize_columns <- function(df) {
  nms <- names(df)
  
  # Helper: găsește prima coloană care se potrivește cu oricare dintre pattern-uri
  pick <- function(patterns) {
    idx <- which(stringr::str_detect(stringr::str_to_lower(nms), paste(patterns, collapse="|")))
    if (length(idx) == 0) return(NA_character_)
    nms[idx[1]]
  }
  
  col_id   <- pick(c("^id$", "id\\b"))
  col_turn <- pick(c("turnover", "cifr", "cifra"))
  col_np   <- pick(c("netprofit", "profit", "rezultat"))
  col_emp  <- pick(c("employees", "angajat"))
  col_lit  <- pick(c("^litigation$", "total\\s*dos", "totaldos", "procese", "dosare\\s*litigii(?!\\s*anaf)"))
  col_tax  <- pick(c("taxauthoritylitigation", "anaf", "dosare\\s*litigii\\s*anaf"))
  
  # Verificări minime ca să nu ruleze pe date greșite
  if (is.na(col_turn) || is.na(col_np) || is.na(col_emp) || is.na(col_lit) || is.na(col_tax)) {
    stop(
      "Nu am putut identifica toate coloanele necesare. ",
      "Aștept echivalente pentru: Turnover/NetProfit/Employees/Litigation/TaxAuthorityLitigation. ",
      "Nume coloane detectate: ", paste(nms, collapse = ", ")
    )
  }
  
  # Redenumire în standardul aplicației
  df2 <- df %>%
    dplyr::rename(
      Turnover = !!col_turn,
      NetProfit = !!col_np,
      Employees = !!col_emp,
      Litigation = !!col_lit,
      TaxAuthorityLitigation = !!col_tax
    )
  
  if (!is.na(col_id) && col_id != "ID") {
    df2 <- df2 %>% dplyr::rename(ID = !!col_id)
  } else if (!("ID" %in% names(df2))) {
    df2$ID <- seq_len(nrow(df2))
  }
  
  # Conversii numerice robuste (și pentru cazurile cu virgulă)
  df2 <- df2 %>%
    dplyr::mutate(
      Turnover = .standardize_num(Turnover),
      NetProfit = .standardize_num(NetProfit),
      Employees = .standardize_num(Employees),
      Litigation = .standardize_num(Litigation),
      TaxAuthorityLitigation = .standardize_num(TaxAuthorityLitigation)
    )
  
  df2
}