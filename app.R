# ============================================================
# app.R — MEI-grade Analytics Dashboard
# Mastercard Economics Institute Portfolio Project
# ============================================================

source("global.R")

# ============================================================
# HELPER: plotly theme
# ============================================================

mc_theme <- function(p) {
  p %>% layout(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font          = list(color = "#ecf0f1", family = "Arial, sans-serif"),
    xaxis         = list(gridcolor = "rgba(255,255,255,0.08)", zerolinecolor = "rgba(255,255,255,0.1)"),
    yaxis         = list(gridcolor = "rgba(255,255,255,0.08)", zerolinecolor = "rgba(255,255,255,0.1)"),
    legend        = list(bgcolor = "rgba(0,0,0,0.3)", bordercolor = "rgba(255,255,255,0.1)", borderwidth = 1),
    margin        = list(l = 50, r = 20, t = 40, b = 50)
  )
}

# ============================================================
# UI
# ============================================================

ui <- dashboardPage(
  skin = "black",
  
  # ---- HEADER ------------------------------------------------
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2a/Mastercard-logo.svg/120px-Mastercard-logo.svg.png",
               height = "28px", style = "margin-right:8px; vertical-align:middle;"),
      "MEI Analytics Platform"
    ),
    titleWidth = 280
  ),
  
  # ---- SIDEBAR -----------------------------------------------
  dashboardSidebar(
    width = 260,
    tags$head(
      tags$style(HTML("
        .skin-black .main-sidebar { background-color: #0d1117; }
        .skin-black .sidebar-menu > li.active > a,
        .skin-black .sidebar-menu > li > a:hover { background-color: #1a1f2e; border-left: 3px solid #EB001B; }
        .sidebar-menu .treeview-menu { background-color: #080d14; }
        .info-box-icon { border-radius: 4px 0 0 4px; }
        .content-wrapper, .main-footer { background-color: #0f1420; }
        .box { background-color: #1a1f2e; border-top: 3px solid #EB001B; border-radius: 6px; }
        .box-header { background-color: #1a1f2e; color: #ecf0f1; }
        .box-title { color: #ecf0f1; font-weight: 600; }
        h3, h4, h5 { color: #ecf0f1; }
        .nav-tabs-custom { background: #1a1f2e; }
        .nav-tabs-custom > .nav-tabs > li.active > a { background: #EB001B; color: #fff; border: none; }
        .nav-tabs-custom > .nav-tabs > li > a { color: #aaa; }
        .dataTables_wrapper { color: #ecf0f1; }
        table.dataTable thead { background-color: #0d1117; color: #ecf0f1; }
        table.dataTable tbody tr { background-color: #1a1f2e !important; color: #ecf0f1; }
        table.dataTable tbody tr:hover { background-color: #252d3d !important; }
        .selectize-input, .selectize-dropdown { background-color: #1a1f2e !important; color: #ecf0f1; }
        .slider-container { color: #ecf0f1; }
        label { color: #b0b8c8; }
      "))
    ),
    sidebarMenu(
      id = "sidebar",
      menuItem("Executive Dashboard", tabName = "exec", icon = icon("chart-line"),
               menuSubItem("Overview",        tabName = "exec_overview",    icon = icon("gauge")),
               menuSubItem("KPI Monitoring",  tabName = "exec_kpi",         icon = icon("chart-bar")),
               menuSubItem("Model Comparison",tabName = "exec_compare",     icon = icon("scale-balanced"))
      ),
      menuItem("Spam Analytics", tabName = "spam", icon = icon("envelope-circle-check"),
               menuSubItem("ROC Analysis",       tabName = "spam_roc",   icon = icon("chart-area")),
               menuSubItem("Confusion Matrix",   tabName = "spam_cm",    icon = icon("th")),
               menuSubItem("Feature Importance", tabName = "spam_feat",  icon = icon("list-ol")),
               menuSubItem("Prediction Explorer",tabName = "spam_pred",  icon = icon("magnifying-glass"))
      ),
      menuItem("Loan Analytics", tabName = "loan", icon = icon("building-columns"),
               menuSubItem("K Optimization",      tabName = "loan_k",    icon = icon("sliders")),
               menuSubItem("Approval Insights",   tabName = "loan_appr", icon = icon("check-circle")),
               menuSubItem("Correlation Analysis",tabName = "loan_corr", icon = icon("project-diagram")),
               menuSubItem("Prediction Simulator",tabName = "loan_sim",  icon = icon("robot"))
      ),
      menuItem("Diagnostics", tabName = "diag", icon = icon("stethoscope"),
               menuSubItem("Model Drift",        tabName = "diag_drift", icon = icon("arrow-trend-down")),
               menuSubItem("Feature Correlation",tabName = "diag_fcorr", icon = icon("network-wired")),
               menuSubItem("Class Balance",      tabName = "diag_class", icon = icon("scale-unbalanced"))
      ),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"),
               menuSubItem("Interactive Tables", tabName = "data_table",  icon = icon("database")),
               menuSubItem("Filtering",          tabName = "data_filter", icon = icon("filter")),
               menuSubItem("CSV Download",       tabName = "data_dl",     icon = icon("file-csv"))
      )
    )
  ),
  
  # ---- BODY --------------------------------------------------
  dashboardBody(
    tabItems(
      
      # ========================================================
      # EXEC — OVERVIEW
      # ========================================================
      tabItem(tabName = "exec_overview",
              fluidRow(
                infoBox("Best Spam Model", paste0(spam_metrics$Model[which.max(spam_metrics$Accuracy)],
                                                  " — ", round(max(spam_metrics$Accuracy)*100,1), "%"),
                        icon = icon("envelope"), color = "red", fill = TRUE, width = 3),
                infoBox("Best Loan Model", paste0(loan_metrics$Model[which.max(loan_metrics$Accuracy)],
                                                  " — ", round(max(loan_metrics$Accuracy)*100,1), "%"),
                        icon = icon("building-columns"), color = "orange", fill = TRUE, width = 3),
                infoBox("Best Spam AUC", round(max(spam_metrics$AUC), 3),
                        icon = icon("chart-area"), color = "navy", fill = TRUE, width = 3),
                infoBox("Best Loan AUC", round(max(loan_metrics$AUC), 3),
                        icon = icon("chart-area"), color = "teal", fill = TRUE, width = 3)
              ),
              fluidRow(
                box(title = "Platform Overview", width = 12, solidHeader = TRUE, status = "danger",
                    tags$div(style = "color:#ecf0f1; font-size:15px; line-height:1.9;",
                             tags$h4(style="color:#EB001B;", "Mastercard Economics Institute — ML Analytics Platform"),
                             tags$p("This platform delivers production-grade machine learning analytics across two domains:"),
                             tags$ul(
                               tags$li(tags$b("Email Spam Classification:"), " Logistic Regression & Ridge (glmnet) with ROC, confusion matrix, and feature importance diagnostics."),
                               tags$li(tags$b("Loan Approval Prediction:"), " K-Nearest Neighbours & Logistic Regression with k-tuning, approval insights, and interactive simulation.")
                             ),
                             tags$p("Navigate via the sidebar to explore all analytical panels. All models are trained on held-out test sets with cross-validated hyperparameter selection.")
                    )
                )
              ),
              fluidRow(
                box(title = "All Models — Metric Overview", width = 7, solidHeader = TRUE, status = "danger",
                    plotlyOutput("exec_radar", height = "360px")
                ),
                box(title = "Prediction Distribution", width = 5, solidHeader = TRUE, status = "warning",
                    plotlyOutput("exec_pie", height = "360px")
                )
              )
      ),
      
      # ========================================================
      # EXEC — KPI MONITORING
      # ========================================================
      tabItem(tabName = "exec_kpi",
              fluidRow(
                box(title = "Spam Model KPI Trends (Simulated Monthly)", width = 6,
                    solidHeader = TRUE, status = "danger",
                    plotlyOutput("kpi_spam_trend", height = "320px")),
                box(title = "Loan Model KPI Trends (Simulated Monthly)", width = 6,
                    solidHeader = TRUE, status = "warning",
                    plotlyOutput("kpi_loan_trend", height = "320px"))
              ),
              fluidRow(
                box(title = "Spam — All Metrics", width = 6, solidHeader = TRUE, status = "danger",
                    DTOutput("kpi_spam_table")),
                box(title = "Loan — All Metrics", width = 6, solidHeader = TRUE, status = "warning",
                    DTOutput("kpi_loan_table"))
              )
      ),
      
      # ========================================================
      # EXEC — MODEL COMPARISON
      # ========================================================
      tabItem(tabName = "exec_compare",
              fluidRow(
                box(title = "Select Metric", width = 12, solidHeader = TRUE, status = "danger",
                    selectInput("compare_metric", NULL,
                                choices = c("Accuracy","Precision","Recall","F1","AUC"),
                                selected = "Accuracy", width = "200px")
                )
              ),
              fluidRow(
                box(title = "Model Comparison — Bar Chart", width = 8,
                    solidHeader = TRUE, status = "danger",
                    plotlyOutput("compare_bar", height = "380px")),
                box(title = "Gauge — Best Accuracy", width = 4,
                    solidHeader = TRUE, status = "warning",
                    plotlyOutput("gauge_acc", height = "200px"),
                    plotlyOutput("gauge_auc", height = "200px"))
              )
      ),
      
      # ========================================================
      # SPAM — ROC
      # ========================================================
      tabItem(tabName = "spam_roc",
              fluidRow(
                box(title = "ROC Curves — Spam Models", width = 8,
                    solidHeader = TRUE, status = "danger",
                    plotlyOutput("spam_roc_plot", height = "420px")),
                box(title = "AUC Summary", width = 4,
                    solidHeader = TRUE, status = "warning",
                    tags$div(style = "padding:20px;",
                             tags$h4(style="color:#EB001B;", "Area Under Curve"),
                             tags$hr(),
                             tags$p(style="font-size:18px; color:#ecf0f1;",
                                    tags$b("Logistic: "), round(as.numeric(auc(roc_log)), 4)),
                             tags$p(style="font-size:18px; color:#ecf0f1;",
                                    tags$b("Ridge: "), round(as.numeric(auc(roc_ridge)), 4)),
                             tags$hr(),
                             tags$p(style="color:#b0b8c8; font-size:13px;",
                                    "A model with AUC > 0.85 is considered strong for production deployment. ",
                                    "AUC = 1.0 is perfect; AUC = 0.5 is random.")
                    )
                )
              ),
              fluidRow(
                box(title = "Ridge CV — Lambda vs AUC", width = 12,
                    solidHeader = TRUE, status = "danger",
                    plotlyOutput("spam_cv_lambda", height = "300px"))
              )
      ),
      
      # ========================================================
      # SPAM — CONFUSION MATRIX
      # ========================================================
      tabItem(tabName = "spam_cm",
              fluidRow(
                box(title = "Model", width = 3, solidHeader = TRUE, status = "danger",
                    selectInput("cm_model", NULL,
                                choices = c("Logistic","Ridge"), selected = "Logistic")
                )
              ),
              fluidRow(
                box(title = "Confusion Matrix Heatmap", width = 7,
                    solidHeader = TRUE, status = "danger",
                    plotlyOutput("spam_cm_heatmap", height = "400px")),
                box(title = "Classification Metrics", width = 5,
                    solidHeader = TRUE, status = "warning",
                    tableOutput("spam_cm_stats"))
              )
      ),
      
      # ========================================================
      # SPAM — FEATURE IMPORTANCE
      # ========================================================
      tabItem(tabName = "spam_feat",
              fluidRow(
                box(title = "Feature Importance — Logistic Regression (|coefficient|)", width = 8,
                    solidHeader = TRUE, status = "danger",
                    plotlyOutput("spam_feat_bar", height = "420px")),
                box(title = "Top Features", width = 4,
                    solidHeader = TRUE, status = "warning",
                    DTOutput("spam_feat_table"))
              ),
              fluidRow(
                box(title = "Probability Distribution by Class", width = 12,
                    solidHeader = TRUE, status = "danger",
                    plotlyOutput("spam_prob_hist", height = "300px"))
              )
      ),
      
      # ========================================================
      # SPAM — PREDICTION EXPLORER
      # ========================================================
      tabItem(tabName = "spam_pred",
              fluidRow(
                box(title = "Filters", width = 3, solidHeader = TRUE, status = "danger",
                    selectInput("pred_model_sel", "Model", choices = c("Logistic","Ridge")),
                    sliderInput("prob_range", "Probability Range", 0, 1, value = c(0, 1), step = 0.05),
                    selectInput("actual_filter", "Actual Class",
                                choices = c("All","spam","ham"), selected = "All")
                ),
                box(title = "Prediction Explorer", width = 9,
                    solidHeader = TRUE, status = "danger",
                    plotlyOutput("spam_scatter", height = "380px"))
              ),
              fluidRow(
                box(title = "Prediction Records", width = 12,
                    solidHeader = TRUE, status = "warning",
                    DTOutput("spam_pred_table"))
              )
      ),
      
      # ========================================================
      # LOAN — K OPTIMIZATION
      # ========================================================
      tabItem(tabName = "loan_k",
              fluidRow(
                infoBox("Best K",  best_k,     icon = icon("k"), color = "red",    fill = TRUE, width = 4),
                infoBox("Best Acc", paste0(round(max(knn_acc_vec)*100,1),"%"), icon = icon("bullseye"), color = "orange", fill = TRUE, width = 4),
                infoBox("Best AUC", round(max(knn_auc_vec, na.rm=TRUE),3), icon = icon("chart-area"), color = "teal", fill = TRUE, width = 4)
              ),
              fluidRow(
                box(title = "Accuracy vs K", width = 6, solidHeader = TRUE, status = "danger",
                    plotlyOutput("knn_acc_plot", height = "350px")),
                box(title = "AUC vs K", width = 6, solidHeader = TRUE, status = "warning",
                    plotlyOutput("knn_auc_plot", height = "350px"))
              ),
              fluidRow(
                box(title = "K Tuning Results Table", width = 12,
                    solidHeader = TRUE, status = "danger",
                    DTOutput("knn_k_table"))
              )
      ),
      
      # ========================================================
      # LOAN — APPROVAL INSIGHTS
      # ========================================================
      tabItem(tabName = "loan_appr",
              fluidRow(
                box(title = "Approval Distribution", width = 5,
                    solidHeader = TRUE, status = "danger",
                    plotlyOutput("loan_pie", height = "340px")),
                box(title = "Income by Approval Status", width = 7,
                    solidHeader = TRUE, status = "warning",
                    plotlyOutput("loan_income_box", height = "340px"))
              ),
              fluidRow(
                box(title = "CIBIL Score Distribution", width = 6,
                    solidHeader = TRUE, status = "danger",
                    plotlyOutput("loan_cibil_hist", height = "300px")),
                box(title = "Loan Amount Distribution", width = 6,
                    solidHeader = TRUE, status = "warning",
                    plotlyOutput("loan_amt_hist", height = "300px"))
              )
      ),
      
      # ========================================================
      # LOAN — CORRELATION ANALYSIS
      # ========================================================
      tabItem(tabName = "loan_corr",
              fluidRow(
                box(title = "Feature Correlation Heatmap", width = 8,
                    solidHeader = TRUE, status = "danger",
                    plotlyOutput("loan_corr_heatmap", height = "450px")),
                box(title = "Feature Importance — Loan Logistic", width = 4,
                    solidHeader = TRUE, status = "warning",
                    plotlyOutput("loan_feat_bar", height = "450px"))
              )
      ),
      
      # ========================================================
      # LOAN — PREDICTION SIMULATOR
      # ========================================================
      tabItem(tabName = "loan_sim",
              fluidRow(
                box(title = "Input Parameters", width = 4,
                    solidHeader = TRUE, status = "danger",
                    sliderInput("sim_income",     "Annual Income (₹)", 10000, 200000, 55000, step = 1000),
                    sliderInput("sim_loan",       "Loan Amount (₹)",    10000, 500000, 150000, step = 5000),
                    sliderInput("sim_cibil",      "CIBIL Score",         300, 900, 650, step = 10),
                    sliderInput("sim_emp",        "Employment Length (yrs)", 0, 30, 5),
                    sliderInput("sim_assets",     "Assets Value (₹)", 0, 1000000, 200000, step = 10000),
                    sliderInput("sim_deps",       "Dependents", 0, 8, 0),
                    selectInput("sim_term",       "Loan Term (months)", choices = c(12, 24, 36, 60, 120), selected = 36),
                    actionButton("sim_run", "Run Prediction", class = "btn-danger btn-block")
                ),
                box(title = "Prediction Result", width = 8,
                    solidHeader = TRUE, status = "warning",
                    uiOutput("sim_result"),
                    plotlyOutput("sim_gauge", height = "260px"),
                    plotlyOutput("sim_feature_impact", height = "240px")
                )
              )
      ),
      
      # ========================================================
      # DIAGNOSTICS — MODEL DRIFT
      # ========================================================
      tabItem(tabName = "diag_drift",
              fluidRow(
                box(title = "Accuracy Drift Over Time (Simulated)", width = 6,
                    solidHeader = TRUE, status = "danger",
                    plotlyOutput("drift_acc", height = "350px")),
                box(title = "AUC Drift Over Time (Simulated)", width = 6,
                    solidHeader = TRUE, status = "warning",
                    plotlyOutput("drift_auc", height = "350px"))
              ),
              fluidRow(
                box(title = "Drift Monitoring Table", width = 12,
                    solidHeader = TRUE, status = "danger",
                    DTOutput("drift_table"))
              )
      ),
      
      # ========================================================
      # DIAGNOSTICS — FEATURE CORRELATION
      # ========================================================
      tabItem(tabName = "diag_fcorr",
              fluidRow(
                box(title = "Domain", width = 3, solidHeader = TRUE, status = "danger",
                    selectInput("fcorr_domain", NULL,
                                choices = c("Spam" = "spam", "Loan" = "loan"), selected = "spam")
                )
              ),
              fluidRow(
                box(title = "Feature Correlation Heatmap", width = 12,
                    solidHeader = TRUE, status = "danger",
                    plotlyOutput("fcorr_plot", height = "500px"))
              )
      ),
      
      # ========================================================
      # DIAGNOSTICS — CLASS BALANCE
      # ========================================================
      tabItem(tabName = "diag_class",
              fluidRow(
                box(title = "Spam — Class Balance", width = 6,
                    solidHeader = TRUE, status = "danger",
                    plotlyOutput("class_spam_bar", height = "320px")),
                box(title = "Loan — Class Balance", width = 6,
                    solidHeader = TRUE, status = "warning",
                    plotlyOutput("class_loan_bar", height = "320px"))
              ),
              fluidRow(
                box(title = "Class Imbalance Ratios", width = 12,
                    solidHeader = TRUE, status = "danger",
                    fluidRow(
                      valueBox(table(spam_raw$spam)["spam"], "Spam emails", icon = icon("envelope"), color = "red", width = 3),
                      valueBox(table(spam_raw$spam)["ham"],  "Ham emails",  icon = icon("envelope-open"), color = "green", width = 3),
                      valueBox(table(loan_clean$status)["Approved"],  "Approved loans",  icon = icon("check"), color = "orange", width = 3),
                      valueBox(table(loan_clean$status)["Rejected"],  "Rejected loans",  icon = icon("times"), color = "red", width = 3)
                    )
                )
              )
      ),
      
      # ========================================================
      # DATA EXPLORER — TABLES
      # ========================================================
      tabItem(tabName = "data_table",
              fluidRow(
                box(title = "Dataset", width = 3, solidHeader = TRUE, status = "danger",
                    selectInput("data_sel", NULL,
                                choices = c("Spam (raw)"="spam_raw","Loan (clean)"="loan_clean",
                                            "Spam predictions"="spam_pred","Loan predictions"="loan_pred"),
                                selected = "spam_raw")
                )
              ),
              fluidRow(
                box(title = "Interactive Data Table", width = 12,
                    solidHeader = TRUE, status = "danger",
                    DTOutput("data_dt"))
              )
      ),
      
      # ========================================================
      # DATA EXPLORER — FILTERING
      # ========================================================
      tabItem(tabName = "data_filter",
              fluidRow(
                box(title = "Loan Data — Advanced Filter", width = 3,
                    solidHeader = TRUE, status = "danger",
                    sliderInput("filt_income", "Income Range", 0, 200000,
                                c(min(loan_clean$income), max(loan_clean$income)), step = 1000),
                    sliderInput("filt_cibil", "CIBIL Range", 300, 900,
                                c(min(loan_clean$cibil_score), max(loan_clean$cibil_score)), step = 10),
                    selectInput("filt_status", "Status", choices = c("All","Approved","Rejected")),
                    actionButton("filt_apply", "Apply Filter", class = "btn-danger btn-sm btn-block")
                ),
                box(title = "Filtered Results", width = 9,
                    solidHeader = TRUE, status = "warning",
                    DTOutput("filt_table"))
              )
      ),
      
      # ========================================================
      # DATA EXPLORER — CSV DOWNLOAD
      # ========================================================
      tabItem(tabName = "data_dl",
              fluidRow(
                box(title = "Download Datasets", width = 12,
                    solidHeader = TRUE, status = "danger",
                    fluidRow(
                      column(4,
                             tags$div(style="text-align:center; padding:30px;",
                                      tags$h4(style="color:#ecf0f1;", "Spam Dataset"),
                                      tags$p(style="color:#b0b8c8;", paste(nrow(spam_raw), "records,", ncol(spam_raw), "columns")),
                                      downloadButton("dl_spam", "Download Spam CSV", class = "btn-danger")
                             )
                      ),
                      column(4,
                             tags$div(style="text-align:center; padding:30px;",
                                      tags$h4(style="color:#ecf0f1;", "Loan Dataset"),
                                      tags$p(style="color:#b0b8c8;", paste(nrow(loan_clean), "records,", ncol(loan_clean), "columns")),
                                      downloadButton("dl_loan", "Download Loan CSV", class = "btn-warning")
                             )
                      ),
                      column(4,
                             tags$div(style="text-align:center; padding:30px;",
                                      tags$h4(style="color:#ecf0f1;", "Loan Predictions"),
                                      tags$p(style="color:#b0b8c8;", paste(nrow(loan_pred_df), "records")),
                                      downloadButton("dl_preds", "Download Predictions CSV", class = "btn-success")
                             )
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Model Metrics Export", width = 12,
                    solidHeader = TRUE, status = "warning",
                    fluidRow(
                      column(6,
                             tags$div(style="text-align:center; padding:20px;",
                                      downloadButton("dl_spam_metrics", "Download Spam Metrics", class = "btn-danger")
                             )
                      ),
                      column(6,
                             tags$div(style="text-align:center; padding:20px;",
                                      downloadButton("dl_loan_metrics", "Download Loan Metrics", class = "btn-warning")
                             )
                      )
                    )
                )
              )
      )
      
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ---- EXEC OVERVIEW -----------------------------------------
  
  output$exec_radar <- renderPlotly({
    theta_vars <- c("Accuracy","Precision","Recall","F1","AUC","Accuracy")
    fig <- plot_ly(type = "scatterpolar", fill = "toself")
    cols <- c(mc_red, mc_orange, "#3498db", "#2ecc71")
    for (i in 1:nrow(all_metrics)) {
      r_vals <- c(all_metrics$Accuracy[i], all_metrics$Precision[i],
                  all_metrics$Recall[i], all_metrics$F1[i],
                  all_metrics$AUC[i], all_metrics$Accuracy[i])
      fig <- fig %>% add_trace(
        r = r_vals, theta = theta_vars,
        name  = paste(all_metrics$Domain[i], all_metrics$Model[i]),
        line  = list(color = cols[i]),
        fillcolor = paste0(substr(cols[i], 1, 7), "33")
      )
    }
    fig %>% layout(
      polar = list(
        radialaxis = list(visible = TRUE, range = c(0.5, 1),
                          gridcolor = "rgba(255,255,255,0.1)", color = "#ecf0f1"),
        angularaxis = list(color = "#ecf0f1")
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font   = list(color = "#ecf0f1"),
      legend = list(bgcolor = "rgba(0,0,0,0.3)", font = list(color = "#ecf0f1")),
      margin = list(l = 30, r = 30, t = 20, b = 20)
    )
  })
  
  output$exec_pie <- renderPlotly({
    plot_ly(
      labels = c("Spam", "Ham", "Approved", "Rejected"),
      values = c(table(spam_raw$spam)["spam"], table(spam_raw$spam)["ham"],
                 table(loan_clean$status)["Approved"], table(loan_clean$status)["Rejected"]),
      type   = "pie",
      hole   = 0.4,
      marker = list(colors = c(mc_red, mc_orange, "#2ecc71", "#e74c3c"),
                    line = list(color = "#0d1117", width = 2))
    ) %>% mc_theme() %>%
      layout(title = list(text = "Data Distribution", font = list(color = "#ecf0f1")))
  })
  
  # ---- KPI MONITORING ----------------------------------------
  
  output$kpi_spam_trend <- renderPlotly({
    plot_ly(drift_df, x = ~Month) %>%
      add_lines(y = ~Spam_Acc, name = "Spam Accuracy", line = list(color = mc_red, width = 2)) %>%
      add_lines(y = ~Spam_AUC, name = "Spam AUC",      line = list(color = mc_orange, width = 2, dash = "dot")) %>%
      mc_theme() %>%
      layout(yaxis = list(range = c(0.6, 1), title = "Score"),
             xaxis = list(title = "Month"))
  })
  
  output$kpi_loan_trend <- renderPlotly({
    plot_ly(drift_df, x = ~Month) %>%
      add_lines(y = ~Loan_Acc, name = "Loan Accuracy", line = list(color = "#3498db", width = 2)) %>%
      add_lines(y = ~Loan_AUC, name = "Loan AUC",      line = list(color = "#2ecc71", width = 2, dash = "dot")) %>%
      mc_theme() %>%
      layout(yaxis = list(range = c(0.6, 1), title = "Score"),
             xaxis = list(title = "Month"))
  })
  
  output$kpi_spam_table <- renderDT({
    datatable(spam_metrics, options = list(dom = "t", pageLength = 5),
              rownames = FALSE) %>%
      formatStyle(columns = colnames(spam_metrics), color = "#ecf0f1", backgroundColor = "#1a1f2e")
  })
  
  output$kpi_loan_table <- renderDT({
    datatable(loan_metrics, options = list(dom = "t", pageLength = 5),
              rownames = FALSE) %>%
      formatStyle(columns = colnames(loan_metrics), color = "#ecf0f1", backgroundColor = "#1a1f2e")
  })
  
  # ---- MODEL COMPARISON --------------------------------------
  
  output$compare_bar <- renderPlotly({
    metric <- input$compare_metric
    df     <- all_metrics %>% mutate(label = paste(Domain, Model))
    plot_ly(df, x = ~label, y = as.formula(paste0("~", metric)),
            color = ~Domain, colors = c("Loan" = mc_orange, "Spam" = mc_red),
            type = "bar") %>%
      mc_theme() %>%
      layout(xaxis = list(title = "Model"), yaxis = list(title = metric, range = c(0, 1)),
             barmode = "group")
  })
  
  output$gauge_acc <- renderPlotly({
    val <- round(max(all_metrics$Accuracy) * 100, 1)
    plot_ly(type = "indicator", mode = "gauge+number",
            value = val,
            title = list(text = "Best Accuracy %", font = list(color = "#ecf0f1")),
            number = list(font = list(color = "#ecf0f1")),
            gauge = list(
              axis  = list(range = list(0, 100), tickcolor = "#ecf0f1"),
              bar   = list(color = mc_red),
              steps = list(
                list(range = c(0,  60), color = "#2c3e50"),
                list(range = c(60, 80), color = "#34495e"),
                list(range = c(80,100), color = "#3d566e")
              )
            )) %>%
      layout(paper_bgcolor = "rgba(0,0,0,0)", font = list(color = "#ecf0f1"),
             margin = list(l = 20, r = 20, t = 40, b = 10))
  })
  
  output$gauge_auc <- renderPlotly({
    val <- round(max(all_metrics$AUC), 3)
    plot_ly(type = "indicator", mode = "gauge+number",
            value = val,
            title = list(text = "Best AUC", font = list(color = "#ecf0f1")),
            number = list(font = list(color = "#ecf0f1")),
            gauge = list(
              axis  = list(range = list(0, 1), tickcolor = "#ecf0f1"),
              bar   = list(color = mc_orange),
              steps = list(
                list(range = c(0,   0.5), color = "#2c3e50"),
                list(range = c(0.5, 0.8), color = "#34495e"),
                list(range = c(0.8, 1.0), color = "#3d566e")
              )
            )) %>%
      layout(paper_bgcolor = "rgba(0,0,0,0)", font = list(color = "#ecf0f1"),
             margin = list(l = 20, r = 20, t = 40, b = 10))
  })
  
  # ---- SPAM ROC ----------------------------------------------
  
  output$spam_roc_plot <- renderPlotly({
    roc_log_df   <- data.frame(FPR = 1 - roc_log$specificities,   TPR = roc_log$sensitivities)
    roc_ridge_df <- data.frame(FPR = 1 - roc_ridge$specificities, TPR = roc_ridge$sensitivities)
    
    plot_ly() %>%
      add_lines(data = roc_log_df,   x = ~FPR, y = ~TPR, name = paste("Logistic (AUC =", round(auc(roc_log),3),")"),
                line = list(color = mc_red,    width = 2.5)) %>%
      add_lines(data = roc_ridge_df, x = ~FPR, y = ~TPR, name = paste("Ridge (AUC =",    round(auc(roc_ridge),3),")"),
                line = list(color = mc_orange, width = 2.5)) %>%
      add_lines(x = c(0,1), y = c(0,1), name = "Random", line = list(color = "grey", dash = "dash", width = 1)) %>%
      mc_theme() %>%
      layout(xaxis = list(title = "False Positive Rate (1 - Specificity)"),
             yaxis = list(title = "True Positive Rate (Sensitivity)"))
  })
  
  output$spam_cv_lambda <- renderPlotly({
    plot_ly(cv_results_spam, x = ~lambda, y = ~mean_auc, type = "scatter", mode = "lines+markers",
            name = "Mean AUC", line = list(color = mc_red, width = 2)) %>%
      add_ribbons(ymin = ~lower, ymax = ~upper, fillcolor = paste0(mc_red, "33"),
                  line = list(color = "transparent"), name = "CV Band") %>%
      mc_theme() %>%
      layout(xaxis = list(title = "log(Lambda)"),
             yaxis = list(title = "Cross-validated AUC"))
  })
  
  # ---- SPAM CONFUSION MATRIX ---------------------------------
  
  cm_reactive <- reactive({
    if (input$cm_model == "Logistic") cm_log else cm_ridge
  })
  
  output$spam_cm_heatmap <- renderPlotly({
    cm  <- cm_reactive()
    df  <- as.data.frame(cm$table)
    mat <- matrix(df$Freq, nrow = 2, dimnames = list(levels(df$Prediction), levels(df$Reference)))
    
    plot_ly(
      x = rownames(mat), y = colnames(mat),
      z = t(mat),
      type = "heatmap",
      colorscale = list(c(0,"#0d1117"), c(0.5,"#8B0000"), c(1, mc_red)),
      showscale = TRUE,
      text = t(mat), texttemplate = "<b>%{text}</b>",
      hovertemplate = "Predicted: %{x}<br>Actual: %{y}<br>Count: %{z}<extra></extra>"
    ) %>%
      mc_theme() %>%
      layout(xaxis = list(title = "Predicted"),
             yaxis = list(title = "Actual"),
             title = list(text = paste("Confusion Matrix —", input$cm_model),
                          font = list(color = "#ecf0f1")))
  })
  
  output$spam_cm_stats <- renderTable({
    cm <- cm_reactive()
    data.frame(
      Metric = c("Accuracy","Sensitivity","Specificity","Precision","Recall","F1","Kappa"),
      Value  = round(c(cm$overall["Accuracy"], cm$byClass["Sensitivity"],
                       cm$byClass["Specificity"], cm$byClass["Precision"],
                       cm$byClass["Recall"],      cm$byClass["F1"],
                       cm$overall["Kappa"]), 4)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ---- SPAM FEATURE IMPORTANCE --------------------------------
  
  output$spam_feat_bar <- renderPlotly({
    plot_ly(coef_df, x = ~Importance, y = ~reorder(Feature, Importance),
            type = "bar", orientation = "h",
            marker = list(color = mc_red, line = list(color = "#ecf0f1", width = 0.3))) %>%
      mc_theme() %>%
      layout(xaxis = list(title = "|Coefficient|"),
             yaxis = list(title = ""))
  })
  
  output$spam_feat_table <- renderDT({
    datatable(coef_df %>% mutate(Importance = round(Importance, 4)),
              options = list(dom = "t", pageLength = 12), rownames = FALSE) %>%
      formatStyle(columns = c("Feature","Importance"), color = "#ecf0f1", backgroundColor = "#1a1f2e")
  })
  
  output$spam_prob_hist <- renderPlotly({
    plot_ly(spam_prob_df, x = ~probability, color = ~actual,
            colors = c("ham" = mc_orange, "spam" = mc_red),
            type = "histogram", opacity = 0.75, nbinsx = 40) %>%
      mc_theme() %>%
      layout(barmode = "overlay",
             xaxis = list(title = "Predicted Probability"),
             yaxis = list(title = "Count"))
  })
  
  # ---- SPAM PREDICTION EXPLORER ------------------------------
  
  spam_pred_reactive <- reactive({
    probs <- if (input$pred_model_sel == "Logistic") log_probs else ridge_probs
    preds <- if (input$pred_model_sel == "Logistic") log_preds else ridge_preds
    
    df <- data.frame(
      index       = seq_along(probs),
      probability = probs,
      predicted   = as.character(preds),
      actual      = as.character(spam_test$spam),
      correct     = preds == spam_test$spam
    ) %>%
      filter(probability >= input$prob_range[1],
             probability <= input$prob_range[2])
    
    if (input$actual_filter != "All")
      df <- df %>% filter(actual == input$actual_filter)
    df
  })
  
  output$spam_scatter <- renderPlotly({
    df <- spam_pred_reactive()
    plot_ly(df, x = ~index, y = ~probability, color = ~correct,
            colors = c("FALSE" = mc_red, "TRUE" = "#2ecc71"),
            type = "scatter", mode = "markers",
            marker = list(size = 6, opacity = 0.8),
            text = ~paste("Actual:", actual, "<br>Predicted:", predicted),
            hoverinfo = "text+y") %>%
      mc_theme() %>%
      layout(xaxis = list(title = "Observation Index"),
             yaxis = list(title = "Predicted Probability", range = c(0, 1)))
  })
  
  output$spam_pred_table <- renderDT({
    datatable(spam_pred_reactive() %>% mutate(probability = round(probability, 4)),
              options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle("correct", backgroundColor = styleEqual(c(TRUE, FALSE), c("#1a3a1a", "#3a1a1a")),
                  color = "#ecf0f1")
  })
  
  # ---- LOAN K OPTIMIZATION -----------------------------------
  
  output$knn_acc_plot <- renderPlotly({
    plot_ly(knn_k_df, x = ~k, y = ~Accuracy, type = "scatter", mode = "lines+markers",
            line = list(color = mc_red, width = 2),
            marker = list(color = mc_orange, size = 8)) %>%
      add_lines(x = c(best_k, best_k), y = c(min(knn_k_df$Accuracy), max(knn_k_df$Accuracy)),
                name = paste("Best k =", best_k), line = list(color = "#2ecc71", dash = "dash")) %>%
      mc_theme() %>%
      layout(xaxis = list(title = "k"), yaxis = list(title = "Accuracy"))
  })
  
  output$knn_auc_plot <- renderPlotly({
    plot_ly(knn_k_df, x = ~k, y = ~AUC, type = "scatter", mode = "lines+markers",
            line = list(color = mc_orange, width = 2),
            marker = list(color = mc_red, size = 8)) %>%
      mc_theme() %>%
      layout(xaxis = list(title = "k"), yaxis = list(title = "AUC"))
  })
  
  output$knn_k_table <- renderDT({
    datatable(knn_k_df %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
              options = list(pageLength = 14, dom = "t"), rownames = FALSE) %>%
      formatStyle(columns = c("k","Accuracy","AUC"), color = "#ecf0f1", backgroundColor = "#1a1f2e")
  })
  
  # ---- LOAN APPROVAL INSIGHTS --------------------------------
  
  output$loan_pie <- renderPlotly({
    plot_ly(labels = names(loan_dist), values = as.numeric(loan_dist),
            type = "pie", hole = 0.5,
            marker = list(colors = c("#2ecc71", mc_red),
                          line = list(color = "#0d1117", width = 2))) %>%
      mc_theme()
  })
  
  output$loan_income_box <- renderPlotly({
    plot_ly(loan_clean, y = ~income, color = ~status,
            colors = c("Approved" = "#2ecc71", "Rejected" = mc_red),
            type = "box", boxpoints = "suspectedoutliers") %>%
      mc_theme() %>%
      layout(yaxis = list(title = "Income (₹)"), xaxis = list(title = "Status"))
  })
  
  output$loan_cibil_hist <- renderPlotly({
    plot_ly(loan_clean, x = ~cibil_score, color = ~status,
            colors = c("Approved" = "#2ecc71", "Rejected" = mc_red),
            type = "histogram", opacity = 0.8, nbinsx = 30) %>%
      mc_theme() %>%
      layout(barmode = "overlay",
             xaxis = list(title = "CIBIL Score"), yaxis = list(title = "Count"))
  })
  
  output$loan_amt_hist <- renderPlotly({
    plot_ly(loan_clean, x = ~loan_amount, color = ~status,
            colors = c("Approved" = mc_orange, "Rejected" = mc_red),
            type = "histogram", opacity = 0.8, nbinsx = 30) %>%
      mc_theme() %>%
      layout(barmode = "overlay",
             xaxis = list(title = "Loan Amount (₹)"), yaxis = list(title = "Count"))
  })
  
  # ---- LOAN CORRELATION ANALYSIS -----------------------------
  
  output$loan_corr_heatmap <- renderPlotly({
    mat  <- loan_cor
    plot_ly(x = colnames(mat), y = rownames(mat), z = mat,
            type = "heatmap",
            colorscale = list(c(0,"#1a237e"), c(0.5,"#0d1117"), c(1, mc_red)),
            showscale = TRUE,
            text = round(mat, 2), texttemplate = "%{text}",
            hovertemplate = "%{x} × %{y}: %{z:.3f}<extra></extra>") %>%
      mc_theme()
  })
  
  output$loan_feat_bar <- renderPlotly({
    plot_ly(loan_feat_imp, x = ~Importance, y = ~reorder(Feature, Importance),
            type = "bar", orientation = "h",
            marker = list(color = mc_orange)) %>%
      mc_theme() %>%
      layout(xaxis = list(title = "|Coefficient|"), yaxis = list(title = ""))
  })
  
  # ---- LOAN PREDICTION SIMULATOR -----------------------------
  
  sim_data <- eventReactive(input$sim_run, {
    new_obs <- data.frame(
      income           = input$sim_income,
      loan_amount      = input$sim_loan,
      cibil_score      = input$sim_cibil,
      emp_length       = input$sim_emp,
      assets_value     = input$sim_assets,
      loan_term        = as.numeric(input$sim_term),
      no_of_dependents = input$sim_deps
    )
    prob  <- predict(loan_log, new_obs, type = "response")
    label <- ifelse(prob > 0.5, "Approved", "Rejected")
    list(prob = prob, label = label, data = new_obs)
  })
  
  output$sim_result <- renderUI({
    res <- sim_data()
    col <- if (res$label == "Approved") "#2ecc71" else mc_red
    tags$div(style = paste0("text-align:center; padding:20px; border:2px solid ", col,
                            "; border-radius:8px; margin-bottom:15px;"),
             tags$h2(style = paste0("color:", col, ";"), res$label),
             tags$p(style = "color:#b0b8c8;",
                    paste0("Approval probability: ", round(res$prob * 100, 1), "%"))
    )
  })
  
  output$sim_gauge <- renderPlotly({
    res <- sim_data()
    plot_ly(type = "indicator", mode = "gauge+number+delta",
            value = round(res$prob * 100, 1),
            title = list(text = "Approval Probability (%)", font = list(color = "#ecf0f1")),
            number = list(suffix = "%", font = list(color = "#ecf0f1")),
            delta  = list(reference = 50, font = list(color = "#ecf0f1")),
            gauge  = list(
              axis  = list(range = list(0, 100), tickcolor = "#ecf0f1"),
              bar   = list(color = if (res$prob > 0.5) "#2ecc71" else mc_red),
              threshold = list(line = list(color = mc_orange, width = 3), value = 50)
            )) %>%
      layout(paper_bgcolor = "rgba(0,0,0,0)", font = list(color = "#ecf0f1"),
             margin = list(l = 30, r = 30, t = 50, b = 10))
  })
  
  output$sim_feature_impact <- renderPlotly({
    res <- sim_data()
    coefs   <- coef(loan_log)[-1]
    impacts <- coefs * as.numeric(res$data[1, names(coefs)])
    imp_df  <- data.frame(Feature = names(impacts), Impact = impacts) %>%
      arrange(desc(abs(Impact))) %>% head(7)
    
    plot_ly(imp_df, x = ~Impact, y = ~reorder(Feature, Impact),
            type = "bar", orientation = "h",
            marker = list(color = ifelse(imp_df$Impact > 0, "#2ecc71", mc_red))) %>%
      mc_theme() %>%
      layout(xaxis = list(title = "Feature Impact (coef × value)"), yaxis = list(title = ""))
  })
  
  # ---- DIAGNOSTICS: DRIFT ------------------------------------
  
  output$drift_acc <- renderPlotly({
    plot_ly(drift_df, x = ~Month) %>%
      add_lines(y = ~Spam_Acc, name = "Spam", line = list(color = mc_red, width = 2)) %>%
      add_lines(y = ~Loan_Acc, name = "Loan", line = list(color = mc_orange, width = 2)) %>%
      mc_theme() %>%
      layout(yaxis = list(title = "Accuracy", range = c(0.6, 1)))
  })
  
  output$drift_auc <- renderPlotly({
    plot_ly(drift_df, x = ~Month) %>%
      add_lines(y = ~Spam_AUC, name = "Spam", line = list(color = "#3498db", width = 2)) %>%
      add_lines(y = ~Loan_AUC, name = "Loan", line = list(color = "#2ecc71", width = 2)) %>%
      mc_theme() %>%
      layout(yaxis = list(title = "AUC", range = c(0.6, 1)))
  })
  
  output$drift_table <- renderDT({
    datatable(drift_df %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
              options = list(pageLength = 12, dom = "t"), rownames = FALSE) %>%
      formatStyle(columns = colnames(drift_df), color = "#ecf0f1", backgroundColor = "#1a1f2e")
  })
  
  # ---- DIAGNOSTICS: FEATURE CORRELATION ----------------------
  
  output$fcorr_plot <- renderPlotly({
    mat <- if (input$fcorr_domain == "spam") {
      cor(spam_raw[, sapply(spam_raw, is.numeric)], use = "complete.obs")
    } else {
      loan_cor
    }
    plot_ly(x = colnames(mat), y = rownames(mat), z = mat,
            type = "heatmap",
            colorscale = list(c(0,"#1a237e"), c(0.5,"#0d1117"), c(1, mc_red)),
            text = round(mat, 2), texttemplate = "%{text}") %>%
      mc_theme()
  })
  
  # ---- DIAGNOSTICS: CLASS BALANCE ----------------------------
  
  output$class_spam_bar <- renderPlotly({
    df <- as.data.frame(table(spam_raw$spam))
    plot_ly(df, x = ~Var1, y = ~Freq, type = "bar",
            marker = list(color = c(mc_orange, mc_red))) %>%
      mc_theme() %>%
      layout(xaxis = list(title = "Class"), yaxis = list(title = "Count"))
  })
  
  output$class_loan_bar <- renderPlotly({
    df <- as.data.frame(table(loan_clean$status))
    plot_ly(df, x = ~Var1, y = ~Freq, type = "bar",
            marker = list(color = c("#2ecc71", mc_red))) %>%
      mc_theme() %>%
      layout(xaxis = list(title = "Status"), yaxis = list(title = "Count"))
  })
  
  # ---- DATA EXPLORER: TABLES ---------------------------------
  
  output$data_dt <- renderDT({
    df <- switch(input$data_sel,
                 spam_raw   = spam_raw,
                 loan_clean = loan_clean,
                 spam_pred  = data.frame(
                   actual    = as.character(spam_test$spam),
                   log_pred  = as.character(log_preds),
                   log_prob  = round(log_probs, 3),
                   ridge_pred = as.character(ridge_preds),
                   ridge_prob = round(ridge_probs, 3)
                 ),
                 loan_pred  = loan_pred_df
    )
    datatable(df, extensions = "Buttons",
              options = list(pageLength = 15, scrollX = TRUE,
                             dom = "Bfrtip",
                             buttons = c("copy", "csv", "excel")),
              rownames = FALSE)
  })
  
  # ---- DATA EXPLORER: FILTERING ------------------------------
  
  filtered_loan <- eventReactive(input$filt_apply, {
    df <- loan_clean %>%
      filter(income      >= input$filt_income[1], income      <= input$filt_income[2],
             cibil_score >= input$filt_cibil[1],  cibil_score <= input$filt_cibil[2])
    if (input$filt_status != "All") df <- df %>% filter(status == input$filt_status)
    df
  }, ignoreNULL = FALSE)
  
  output$filt_table <- renderDT({
    datatable(filtered_loan() %>% mutate(across(where(is.numeric), ~ round(.x, 2))),
              options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })
  
  # ---- DATA EXPLORER: DOWNLOADS ------------------------------
  
  output$dl_spam        <- downloadHandler(filename = "spam_data.csv",
                                           content = function(f) write.csv(spam_raw,    f, row.names = FALSE))
  output$dl_loan        <- downloadHandler(filename = "loan_data.csv",
                                           content = function(f) write.csv(loan_clean,  f, row.names = FALSE))
  output$dl_preds       <- downloadHandler(filename = "loan_predictions.csv",
                                           content = function(f) write.csv(loan_pred_df, f, row.names = FALSE))
  output$dl_spam_metrics <- downloadHandler(filename = "spam_metrics.csv",
                                            content = function(f) write.csv(spam_metrics, f, row.names = FALSE))
  output$dl_loan_metrics <- downloadHandler(filename = "loan_metrics.csv",
                                            content = function(f) write.csv(loan_metrics, f, row.names = FALSE))
  
} # end server

shinyApp(ui = ui, server = server)