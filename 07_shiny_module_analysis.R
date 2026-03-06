# =======================================================================
#  R/07_shiny_module_analysis.R  (model robust)
#
#  Presupune adaugarea source("R/08_dashboard_helpers.R") în app.R
#
#  care implică adăugare unui tab nou "Tablou de bord")
#
#  (gauge p90 + tabel risc)
#  
#  Element ID cu focus + tab Firma selectată 
#
# =======================================================================

analysisModuleUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fileInput(ns("file"), "Încarcă fișier Excel", accept = c(".xlsx")),
    shiny::checkboxInput(ns("use_nontax"),
                         "Folosește NonTaxLitigation (Litigation - TaxAuthorityLitigation)",
                         value = TRUE),
    shiny::checkboxInput(ns("nonlinear"),
                         "Permite neliniarități (splines)",
                         value = FALSE),
    shiny::sliderInput(ns("min_total"),
                       "Minim litigii totale (Litigation)",
                       min = 0, max = 20, value = 1, step = 1),
    shiny::sliderInput(ns("min_tax"),
                       "Minim litigii cu ANAF (TaxAuthorityLitigation)",
                       min = 0, max = 20, value = 1, step = 1),
    shiny::verbatimTextOutput(ns("debug_error")),
    shiny::hr(),
    shiny::tabsetPanel(
      shiny::tabPanel("Tablou de bord",
                      shiny::fluidRow(
                        shiny::column(6, shiny::plotOutput(ns("gauge_risk"), height = "220px")),
                        shiny::column(6, shiny::tableOutput(ns("risk_preview")))
                      ),
                      shiny::fluidRow(
                        shiny::column(6, shiny::h4("Elasticități interpretate"),
                                      shiny::verbatimTextOutput(ns("elasticities_text"))),
                        shiny::column(6, shiny::h4("Analiză de sensibilitate"),
                                      shiny::tableOutput(ns("sens_tbl")))
                      ),
                      shiny::fluidRow(
                        shiny::column(12, shiny::plotOutput(ns("partial_plot"), height = "260px"))
                      )
      ),
      
      shiny::tabPanel("Firma selectată",
                      shiny::fluidRow(
                        shiny::column(6, shiny::plotOutput(ns("gauge_firma"), height = "220px")),
                        shiny::column(6, shiny::tableOutput(ns("firma_values")))
                      ),
                      shiny::fluidRow(
                        shiny::column(12, shiny::tableOutput(ns("firma_model_info")))
                      )
      ),
      
      shiny::tabPanel("Descriere",
                      shiny::tableOutput(ns("summary")),
                      shiny::plotOutput(ns("hist"))),
      shiny::tabPanel("Grafice",
                      shiny::plotOutput(ns("scatter_turnover")),
                      shiny::plotOutput(ns("scatter_employees"))),
      shiny::tabPanel("Modele",
                      shiny::tableOutput(ns("aic")),
                      shiny::tableOutput(ns("rr_model")),
                      shiny::verbatimTextOutput(ns("model_note"))),
      
      shiny::tabPanel("Predicții",
                      DT::DTOutput(ns("top_risk_dt")))
    )
  )
}

analysisModuleServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    selected_id <- shiny::reactiveVal(NULL)
    
    data_raw <- shiny::reactive({
      shiny::req(input$file)
      tryCatch({
        df <- read_input_data(input$file$datapath)
        df <- standardize_columns(df)
        df <- preprocess_for_count_models(df)
        df
      }, error = function(e) {
        output$debug_error <- shiny::renderText(paste("Eroare la încărcare/preprocesare:", e$message))
        stop(e)
      })
    })
    
    data_filtered <- shiny::reactive({
      df <- data_raw()
      df <- filter_analysis_sample(df, min_tax = input$min_tax, min_total = input$min_total)
      df <- clean_for_model(df)
      df
    })
    
    fit_pois <- shiny::reactive({
      df <- data_filtered()
      f <- build_formula(df, use_nontax = input$use_nontax, nonlinear = FALSE, y_name = "TaxAuthorityLitigation")
      fit_poisson(df, f)
    })
    
    fit_robust <- shiny::reactive({
      df <- data_filtered()
      robust_fit_negbin(df, use_nontax = input$use_nontax, nonlinear = input$nonlinear)
    })
    
    pred_all <- shiny::reactive({
      df <- data_filtered()
      m  <- fit_robust()$model
      dfp <- predict_expected_counts(m, df)
      dfp <- dfp %>% dplyr::mutate(RiskScore = risk_score_0_100(.))
      dfp
    })
    
    output$top_risk_dt <- DT::renderDT({
      dfp <- pred_all() %>%
        dplyr::arrange(dplyr::desc(pred_TaxAuthorityLitigation)) %>%
        dplyr::select(
          ID, Turnover, NetProfit, Employees, Litigation, TaxAuthorityLitigation,
          pred_TaxAuthorityLitigation, RiskScore
        )
      
      DT::datatable(
        dfp,
        selection = "single",
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })
    
    shiny::observeEvent(input$top_risk_dt_rows_selected, {
      i <- input$top_risk_dt_rows_selected
      if (length(i) == 1) {
        dfp_view <- pred_all() %>%
          dplyr::arrange(dplyr::desc(pred_TaxAuthorityLitigation)) %>%
          dplyr::select(
            ID, Turnover, NetProfit, Employees, Litigation, TaxAuthorityLitigation,
            pred_TaxAuthorityLitigation, RiskScore
          )
        selected_id(dfp_view$ID[i])
      }
    })
    
    # ----------------------
    # TAB: Firma selectată
    # ----------------------
    output$gauge_firma <- shiny::renderPlot({
      dfp <- pred_all()
      sid <- selected_id()
      shiny::req(sid)
      
      score_one <- risk_score_one(dfp$RiskScore, dfp$ID, sid)
      make_gauge_plot(score_one)
    })
    
    output$firma_values <- shiny::renderTable({
      dfp <- pred_all()
      sid <- selected_id()
      shiny::req(sid)
      
      dfp %>%
        dplyr::filter(ID == sid) %>%
        dplyr::select(
          ID,
          Turnover,
          NetProfit,
          Employees,
          Litigation,
          TaxAuthorityLitigation,
          pred_TaxAuthorityLitigation,
          RiskScore
        )
    })
    
    output$firma_model_info <- shiny::renderTable({
      info <- fit_robust()
      sid <- selected_id()
      shiny::req(sid)
      
      data.frame(
        Firma_ID = sid,
        Model_folosit = info$used,
        Observatie = "Valorile afișate sunt cele utilizate în predicție; RiskScore (0–100) este standardizat pe distribuția predicțiilor din eșantionul filtrat.",
        row.names = NULL
      )
    })
    
    # ----------------------
    # TAB: Tablou de bord (agregat)
    # ----------------------
    output$risk_preview <- shiny::renderTable({
      dfp <- pred_all()
      
      dfp %>%
        dplyr::arrange(dplyr::desc(RiskScore)) %>%
        dplyr::select(ID, TaxAuthorityLitigation, pred_TaxAuthorityLitigation, RiskScore) %>%
        head(10)
    })
    
    output$gauge_risk <- shiny::renderPlot({
      dfp <- pred_all()
      scores <- dfp$RiskScore
      score <- as.numeric(stats::quantile(scores, 0.90, na.rm = TRUE))
      make_gauge_plot(score)
    })
    
    output$elasticities_text <- shiny::renderText({
      rr <- tidy_rate_ratios(fit_robust()$model)
      elasticities_economic_text(rr)
    })
    
    output$sens_tbl <- shiny::renderTable({
      df <- data_filtered()
      fits <- fit_nb_original_and_winsor(df, use_nontax = input$use_nontax, nonlinear = input$nonlinear)
      sensitivity_table(fits$m_orig, fits$m_win)
    })
    
    output$partial_plot <- shiny::renderPlot({
      df <- data_filtered()
      m <- fit_robust()$model
      pe <- partial_effect_logturnover(m, df, grid_n = 80)
      plot_partial_effect(pe)
    })
    
    # ----------------------
    # TAB-uri existente (Descriere/Grafice/Modele)
    # ----------------------
    output$summary <- shiny::renderTable({
      summary_counts(data_filtered())
    })
    
    output$hist <- shiny::renderPlot({
      plot_y_distribution(data_filtered())
    })
    
    output$scatter_turnover <- shiny::renderPlot({
      plot_scatter_log(data_filtered(), "Turnover")
    })
    
    output$scatter_employees <- shiny::renderPlot({
      plot_scatter_log(data_filtered(), "Employees")
    })
    
    output$aic <- shiny::renderTable({
      compare_models_aic(fit_pois(), fit_robust()$model)
    })
    
    output$model_note <- shiny::renderText({
      paste("Model folosit:", fit_robust()$used)
    })
    
    output$rr_model <- shiny::renderTable({
      tidy_rate_ratios(fit_robust()$model)
    })
    
    invisible(NULL)
  })
}