library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(dplyr)
library(plotly)
library(ggplot2)
library(scales)
library(shinycssloaders)
library(colourpicker)
library(stats)
library(reshape2)

options(spinner.color = "#3498db", spinner.type = 8)

custom_css <- "
/* Dark Blue Theme with Glassmorphism */
.skin-blue .main-header {
  background: linear-gradient(135deg, #0d1b3d 0%, #1a2e5f 100%);
  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.5);
}

.skin-blue .sidebar {
  background: linear-gradient(180deg, #0f2347 0%, #1a3a52 100%);
  width: 260px;
}

.skin-blue .main-sidebar {
  width: 260px;
  background: linear-gradient(180deg, #0f2347 0%, #1a3a52 100%);
}

.skin-blue .sidebar-menu > li > a {
  color: #ecf0f1;
  border-left: 3px solid transparent;
  transition: all 0.3s ease;
}

.skin-blue .sidebar-menu > li > a:hover {
  background: rgba(52, 152, 219, 0.3);
  border-left-color: #3498db;
  color: #3498db;
}

.skin-blue .sidebar-menu > li.active > a {
  background: rgba(52, 152, 219, 0.5);
  border-left-color: #3498db;
  color: #3498db;
}

.content-wrapper {
  background: linear-gradient(135deg, #0d1b2e 0%, #1a2e4a 100%);
  min-height: calc(100vh - 50px);
}

/* Animated Value Boxes */
.info-box {
  background: rgba(52, 152, 219, 0.1);
  border: 1px solid rgba(52, 152, 219, 0.3);
  border-radius: 12px;
  backdrop-filter: blur(10px);
  color: #ecf0f1;
  box-shadow: 0 8px 32px 0 rgba(52, 152, 219, 0.1);
  transition: all 0.3s ease;
  animation: slideIn 0.5s ease-out;
}

.info-box:hover {
  transform: translateY(-5px);
  box-shadow: 0 12px 48px 0 rgba(52, 152, 219, 0.2);
  background: rgba(52, 152, 219, 0.15);
}

@keyframes slideIn {
  from {
    opacity: 0;
    transform: translateY(20px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

.info-box-number {
  font-size: 32px;
  font-weight: bold;
  color: #3498db;
}

.info-box-text {
  color: #bdc3c7;
  font-size: 14px;
}

/* Card Gradient */
.box {
  background: rgba(26, 46, 74, 0.8);
  border: 1px solid rgba(52, 152, 219, 0.2);
  border-radius: 12px;
  box-shadow: 0 8px 32px 0 rgba(0, 0, 0, 0.3);
  color: #ecf0f1;
}

.box-header {
  background: linear-gradient(135deg, rgba(52, 152, 219, 0.2) 0%, rgba(155, 89, 182, 0.1) 100%);
  border-bottom: 1px solid rgba(52, 152, 219, 0.2);
  border-radius: 12px 12px 0 0;
  padding: 15px;
}

.box-title {
  color: #3498db;
  font-weight: bold;
  font-size: 16px;
}

.box-body {
  border-radius: 0 0 12px 12px;
  padding: 15px;
}

/* DataTable Styling */
.dataTables_wrapper {
  color: #ecf0f1;
}

.dataTables_length, .dataTables_filter, .dataTables_info, .dataTables_paginate {
  color: #bdc3c7 !important;
}

table.dataTable thead th {
  background: linear-gradient(135deg, #3498db 0%, #9b59b6 100%);
  color: #fff;
  border-radius: 6px;
}

table.dataTable tbody tr {
  border-bottom: 1px solid rgba(52, 152, 219, 0.1);
}

table.dataTable tbody tr:hover {
  background-color: rgba(52, 152, 219, 0.2) !important;
}

table.dataTable tbody td {
  color: #ecf0f1 !important;
}

/* Buttons */
.btn-primary {
  background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
  border: none;
  border-radius: 8px;
  color: white;
  transition: all 0.3s ease;
}

.btn-primary:hover {
  transform: translateY(-2px);
  box-shadow: 0 8px 20px rgba(52, 152, 219, 0.4);
}

.btn-success {
  background: linear-gradient(135deg, #27ae60 0%, #229954 100%);
  border: none;
  border-radius: 8px;
}

.btn-danger {
  background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%);
  border: none;
  border-radius: 8px;
}

.btn-warning {
  background: linear-gradient(135deg, #f39c12 0%, #e67e22 100%);
  border: none;
  border-radius: 8px;
}

/* Footer */
.footer {
  background: rgba(15, 35, 71, 0.8);
  color: #bdc3c7;
  padding: 20px;
  text-align: center;
  border-top: 1px solid rgba(52, 152, 219, 0.2);
  margin-top: 40px;
}

/* Text Colors */
h1, h2, h3, h4, h5, h6 {
  color: #3498db;
}

/* Select Input Styling */
.form-control {
  background: rgba(26, 46, 74, 0.6);
  border: 1px solid rgba(52, 152, 219, 0.2);
  color: #ecf0f1;
  border-radius: 8px;
}

.form-control:focus {
  background: rgba(26, 46, 74, 0.8);
  border-color: #3498db;
  color: #ecf0f1;
  box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25);
}

/* Row Coloring by Risk Level */
.critical {
  background-color: rgba(231, 76, 60, 0.2) !important;
  color: #e74c3c;
  font-weight: bold;
  padding: 2px 8px;
  border-radius: 4px;
}

.high {
  background-color: rgba(243, 156, 18, 0.2) !important;
  color: #f39c12;
  font-weight: bold;
  padding: 2px 8px;
  border-radius: 4px;
}

.medium {
  background-color: rgba(155, 89, 182, 0.2) !important;
  color: #9b59b6;
  font-weight: bold;
  padding: 2px 8px;
  border-radius: 4px;
}

.low {
  background-color: rgba(39, 174, 96, 0.2) !important;
  color: #27ae60;
  font-weight: bold;
  padding: 2px 8px;
  border-radius: 4px;
}

/* Tab styling */
.nav-tabs {
  border-bottom: 2px solid rgba(52, 152, 219, 0.2);
}

.nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
  background-color: rgba(52, 152, 219, 0.2);
  border: 1px solid rgba(52, 152, 219, 0.3);
  color: #3498db;
  border-bottom-color: #3498db;
}

.nav-tabs > li > a {
  color: #bdc3c7;
  border-radius: 8px 8px 0 0;
}

.nav-tabs > li > a:hover {
  border-color: rgba(52, 152, 219, 0.3);
  color: #3498db;
}

/* Spinner */
.spinner {
  color: #3498db;
}

/* Data missing notice */
.data-warning {
  background: rgba(243, 156, 18, 0.15);
  border: 1px solid rgba(243, 156, 18, 0.4);
  border-radius: 8px;
  padding: 12px 16px;
  color: #f39c12;
  margin-bottom: 15px;
}
"

# ============================================================
# ------------------  SAMPLE DATA GENERATOR  ------------------
# Used only as a fallback when module2_dataset.csv is missing,
# so the dashboard is never blank. Replace the CSV with real
# data (same column names) to see your own results.
# ============================================================
generate_sample_data <- function(n = 250, seed = 42) {
  set.seed(seed)
  
  resource_types <- c("EC2", "RDS", "S3", "IAM", "VPC", "Lambda")
  regions <- c("us-east-1", "us-west-2", "eu-west-1", "ap-south-1", "ap-southeast-1")
  yes_no <- c("Yes", "No")
  patch_status <- c("Updated", "Outdated")
  
  df <- data.frame(
    Asset_ID = sprintf("AST-%04d", 1:n),
    Resource_Name = paste0(sample(resource_types, n, replace = TRUE), "-",
                           sprintf("%03d", sample(1:999, n, replace = TRUE))),
    Resource_Type = sample(resource_types, n, replace = TRUE),
    Region = sample(regions, n, replace = TRUE),
    Public_Access = sample(yes_no, n, replace = TRUE, prob = c(0.3, 0.7)),
    Open_Ports = sample(0:20, n, replace = TRUE),
    Encryption_Status = sample(yes_no, n, replace = TRUE, prob = c(0.6, 0.4)),
    Patch_Status = sample(patch_status, n, replace = TRUE, prob = c(0.55, 0.45)),
    Logging_Enabled = sample(yes_no, n, replace = TRUE, prob = c(0.65, 0.35)),
    Backup_Enabled = sample(yes_no, n, replace = TRUE, prob = c(0.6, 0.4)),
    IAM_Risk = sample(c("Low", "Medium", "High"), n, replace = TRUE),
    Vulnerability_Count = rpois(n, lambda = 4),
    Misconfiguration_Count = rpois(n, lambda = 2),
    Dependency_Count = sample(0:15, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Encryption_Status was stored as Yes/No above; standardize to No when
  # encryption is "off" so downstream Encryption_Status == "No" checks work.
  # (kept as generated)
  
  # Compute a composite risk score influenced by the misconfiguration signals
  df$Risk_Score <- with(df,
                        pmin(100, pmax(0,
                                       20 * (Public_Access == "Yes") +
                                         15 * (Encryption_Status == "No") +
                                         10 * (Patch_Status == "Outdated") +
                                         8  * (Logging_Enabled == "No") +
                                         7  * (Backup_Enabled == "No") +
                                         3  * Vulnerability_Count +
                                         4  * Misconfiguration_Count +
                                         0.5 * Open_Ports +
                                         rnorm(n, mean = 10, sd = 8)
                        ))
  )
  df$Risk_Score <- round(df$Risk_Score, 1)
  
  df$Risk_Level <- cut(
    df$Risk_Score,
    breaks = c(-Inf, 30, 55, 75, Inf),
    labels = c("Low", "Medium", "High", "Critical")
  )
  df$Risk_Level <- as.character(df$Risk_Level)
  
  df
}

# ============================================================
# --------------------  FLOW DIAGRAM HELPER  -------------------
# Builds a proper top-to-bottom flowchart with rounded boxes and
# arrows using plotly shapes + annotations (replaces the old
# "floating text with thin lines" version that looked empty).
# ============================================================
build_flow_diagram <- function(steps, box_color, fill_rgba, title_text,
                               text_color = "#ecf0f1") {
  n <- length(steps)
  y_positions <- rev(seq_len(n))   # first step at the top
  x_center <- 1
  box_w <- 0.9
  box_h <- 0.62
  
  shapes <- vector("list", n)
  node_annotations <- vector("list", n)
  
  for (i in seq_len(n)) {
    y <- y_positions[i]
    shapes[[i]] <- list(
      type = "rect",
      xref = "x", yref = "y",
      x0 = x_center - box_w / 2, x1 = x_center + box_w / 2,
      y0 = y - box_h / 2,       y1 = y + box_h / 2,
      line = list(color = box_color, width = 2),
      fillcolor = fill_rgba,
      layer = "below"
    )
    node_annotations[[i]] <- list(
      x = x_center, y = y,
      xref = "x", yref = "y",
      text = paste0("<b>", i, ". ", steps[i], "</b>"),
      showarrow = FALSE,
      font = list(color = text_color, size = 14, family = "Arial"),
      align = "center"
    )
  }
  
  arrow_annotations <- list()
  if (n > 1) {
    for (i in seq_len(n - 1)) {
      arrow_annotations[[i]] <- list(
        x = x_center, y = y_positions[i + 1] + box_h / 2 + 0.06,
        ax = x_center, ay = y_positions[i] - box_h / 2 - 0.02,
        xref = "x", yref = "y", axref = "x", ayref = "y",
        showarrow = TRUE,
        arrowhead = 3, arrowsize = 1.4, arrowwidth = 2,
        arrowcolor = box_color
      )
    }
  }
  
  all_annotations <- c(node_annotations, arrow_annotations)
  
  plot_ly() %>%
    layout(
      title = list(text = title_text, font = list(color = "#3498db")),
      shapes = shapes,
      annotations = all_annotations,
      xaxis = list(range = c(0, 2), showgrid = FALSE, showticklabels = FALSE,
                   zeroline = FALSE, fixedrange = TRUE),
      yaxis = list(range = c(0.2, n + 0.8), showgrid = FALSE, showticklabels = FALSE,
                   zeroline = FALSE, fixedrange = TRUE),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0.15)",
      font = list(color = text_color),
      margin = list(l = 20, r = 20, t = 60, b = 20)
    ) %>%
    config(displayModeBar = FALSE)
}

# Helper: find a DT column index (0-based, for JS) for a column name,
# returns NULL (no styling) if the column isn't present, instead of
# crashing on a hardcoded index that no longer matches.
dt_col_index <- function(df, colname) {
  idx <- which(colnames(df) == colname)
  if (length(idx) == 0) return(NULL)
  idx - 1
}

risk_badge_renderer <- function() {
  JS("function(data, type, row) {
        if (data == 'Critical') return '<span class=\"critical\">' + data + '</span>';
        if (data == 'High') return '<span class=\"high\">' + data + '</span>';
        if (data == 'Medium') return '<span class=\"medium\">' + data + '</span>';
        if (data == 'Low') return '<span class=\"low\">' + data + '</span>';
        return data;
      }")
}

ui <- dashboardPage(
  dashboardHeader(
    title = "IaCGuardian",
    titleWidth = 260,
    tags$li(class = "dropdown", tags$a(href = "javascript:void(0)",
                                       icon("bell"), span("Alerts", class = "badge badge-danger")))
  ),
  
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "sidebar",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Summary", tabName = "summary", icon = icon("table")),
      menuItem("Security Findings", tabName = "findings", icon = icon("shield-alt")),
      menuItem("Misconfiguration", tabName = "misconfig", icon = icon("exclamation-triangle")),
      menuItem("Risk Scoring", tabName = "risk_score", icon = icon("sliders-h")),
      menuItem("Risk Analytics", tabName = "analytics", icon = icon("chart-bar")),
      menuItem("Recommendations", tabName = "recommendations", icon = icon("lightbulb")),
      menuItem("Architecture", tabName = "architecture", icon = icon("project-diagram")),
      menuItem("Flow Diagram", tabName = "flow_diagram", icon = icon("code-branch")),
      menuItem("About Project", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    
    tabItems(
      # DASHBOARD TAB
      tabItem(tabName = "dashboard",
              h2("Dashboard Overview", style = "color: #3498db; font-weight: bold;"),
              uiOutput("data_warning"),
              br(),
              fluidRow(
                infoBox("Total Assets", textOutput("total_assets"),
                        icon = icon("server"), color = "blue", width = 2),
                infoBox("Misconfigured Assets", textOutput("misconfig_assets"),
                        icon = icon("exclamation"), color = "red", width = 2),
                infoBox("Critical Assets", textOutput("critical_assets"),
                        icon = icon("skull-crossbones"), color = "orange", width = 2),
                infoBox("Avg Risk Score", textOutput("avg_risk"),
                        icon = icon("chart-pie"), color = "purple", width = 2),
                infoBox("Total Vulnerabilities", textOutput("total_vuln"),
                        icon = icon("bug"), color = "yellow", width = 2),
                infoBox("Public Assets", textOutput("public_assets"),
                        icon = icon("globe"), color = "teal", width = 2)
              ),
              br(),
              fluidRow(
                box(
                  title = "Risk Distribution",
                  status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 6,
                  withSpinner(plotlyOutput("risk_dist_chart"))
                ),
                box(
                  title = "Risk Level Breakdown",
                  status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 6,
                  withSpinner(plotlyOutput("risk_level_pie"))
                )
              ),
              fluidRow(
                box(
                  title = "Top 10 High-Risk Assets",
                  status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 12,
                  div(style = "overflow-x: auto;", withSpinner(DT::dataTableOutput("top_risks_table")))
                )
              )
      ),
      
      # SUMMARY TAB
      tabItem(tabName = "summary",
              h2("Asset Summary", style = "color: #3498db; font-weight: bold;"),
              uiOutput("data_warning_summary"),
              br(),
              fluidRow(
                column(12,
                       box(
                         title = "All Assets Overview",
                         status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 12,
                         div(style = "overflow-x: auto;", withSpinner(DT::dataTableOutput("summary_table")))
                       )
                )
              )
      ),
      
      # SECURITY FINDINGS TAB
      tabItem(tabName = "findings",
              h2("Security Findings", style = "color: #3498db; font-weight: bold;"),
              br(),
              fluidRow(
                column(12,
                       box(
                         title = "Detailed Security Assessment",
                         status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 12,
                         div(style = "overflow-x: auto;", withSpinner(DT::dataTableOutput("findings_table")))
                       )
                )
              )
      ),
      
      # MISCONFIGURATION TAB
      tabItem(tabName = "misconfig",
              h2("Misconfiguration Analysis", style = "color: #3498db; font-weight: bold;"),
              br(),
              fluidRow(
                box(
                  title = "Misconfiguration Frequency",
                  status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 6,
                  withSpinner(plotlyOutput("misconfig_bar"))
                ),
                box(
                  title = "Misconfiguration Distribution",
                  status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 6,
                  withSpinner(plotlyOutput("misconfig_pie"))
                )
              ),
              fluidRow(
                box(
                  title = "Risk Comparison",
                  status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 6,
                  withSpinner(plotlyOutput("risk_comparison"))
                ),
                box(
                  title = "Donut Chart",
                  status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 6,
                  withSpinner(plotlyOutput("misconfig_donut"))
                )
              )
      ),
      
      # RISK SCORING TAB
      tabItem(tabName = "risk_score",
              h2("Risk Scoring Analysis", style = "color: #3498db; font-weight: bold;"),
              br(),
              fluidRow(
                box(
                  title = "Risk Score Distribution (Histogram)",
                  status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 6,
                  withSpinner(plotlyOutput("risk_histogram"))
                ),
                box(
                  title = "Risk by Resource Type",
                  status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 6,
                  withSpinner(plotlyOutput("risk_boxplot"))
                )
              ),
              fluidRow(
                box(
                  title = "Risk Score vs Vulnerability Count",
                  status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 12,
                  withSpinner(plotlyOutput("risk_scatter"))
                )
              )
      ),
      
      # ANALYTICS TAB
      tabItem(tabName = "analytics",
              h2("Risk Analytics", style = "color: #3498db; font-weight: bold;"),
              br(),
              fluidRow(
                box(
                  title = "Correlation Matrix",
                  status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 6,
                  withSpinner(plotlyOutput("correlation_heatmap"))
                ),
                box(
                  title = "KMeans Clustering",
                  status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 6,
                  withSpinner(plotlyOutput("kmeans_cluster"))
                )
              ),
              fluidRow(
                box(
                  title = "Open Ports vs Risk Score",
                  status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 6,
                  withSpinner(plotlyOutput("ports_vs_risk"))
                ),
                box(
                  title = "Dependency Analysis",
                  status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 6,
                  withSpinner(plotlyOutput("dependency_analysis"))
                )
              )
      ),
      
      # RECOMMENDATIONS TAB
      tabItem(tabName = "recommendations",
              h2("Security Recommendations", style = "color: #3498db; font-weight: bold;"),
              br(),
              fluidRow(
                column(12,
                       box(
                         title = "Generated Recommendations",
                         status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 12,
                         withSpinner(uiOutput("recommendations_ui"))
                       )
                )
              )
      ),
      
      # ARCHITECTURE TAB
      tabItem(tabName = "architecture",
              h2("System Architecture", style = "color: #3498db; font-weight: bold;"),
              br(),
              fluidRow(
                column(12,
                       box(
                         title = "IaCGuardian Architecture Diagram",
                         status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 12,
                         withSpinner(plotlyOutput("arch_diagram", height = "700px"))
                       )
                )
              )
      ),
      
      # FLOW DIAGRAM TAB
      tabItem(tabName = "flow_diagram",
              h2("Data Flow Diagram", style = "color: #3498db; font-weight: bold;"),
              br(),
              fluidRow(
                column(12,
                       box(
                         title = "Misconfiguration Detection Flow",
                         status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 12,
                         withSpinner(plotlyOutput("flow_diagram", height = "700px"))
                       )
                )
              )
      ),
      
      # ABOUT TAB
      tabItem(tabName = "about",
              fluidRow(
                column(12,
                       box(
                         title = "About IaCGuardian",
                         status = "primary", solidHeader = FALSE, width = 12,
                         HTML("
                <h3 style='color: #3498db;'>IaCGuardian: Network Simulation of Infrastructure Misconfiguration Blast Radius</h3>
                <br>
                <h4 style='color: #3498db;'>Module 2: Intelligent Misconfiguration Detection & Risk Scoring</h4>
                <br>
                <p style='font-size: 16px; line-height: 1.8; color: #ecf0f1;'>
                  <strong>Purpose:</strong> IaCGuardian is a comprehensive cloud security dashboard that detects infrastructure
                  misconfigurations, scores risk levels, and provides actionable recommendations.
                </p>
                <br>
                <p style='font-size: 16px; line-height: 1.8; color: #ecf0f1;'>
                  <strong>Key Features:</strong>
                </p>
                <ul style='font-size: 16px; line-height: 1.8; color: #ecf0f1;'>
                  <li>Real-time misconfiguration detection across AWS resources (EC2, RDS, S3, IAM, VPC, Lambda)</li>
                  <li>Intelligent risk scoring engine with multi-factor analysis</li>
                  <li>Automated recommendation generation</li>
                  <li>Interactive visualizations and analytics</li>
                  <li>Dependency and blast radius analysis</li>
                  <li>Professional security dashboarding</li>
                </ul>
                <br>
                <p style='font-size: 16px; line-height: 1.8; color: #ecf0f1;'>
                  <strong>Technologies:</strong> R, Shiny, Plotly, ggplot2, DT, shinydashboard, shinycssloaders
                </p>
                <br>
                <p style='font-size: 16px; line-height: 1.8; color: #ecf0f1;'>
                  <strong>Dataset:</strong> module2_dataset.csv (falls back to generated sample data if not found)
                </p>
              ")
                       )
                )
              ),
              fluidRow(
                column(12,
                       tags$div(class = "footer",
                                HTML("<p style='margin: 0;'>&copy; 2024 IaCGuardian. All rights reserved. Cloud Security & Infrastructure as Code Analysis.</p>")
                       )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  data_source_is_sample <- reactiveVal(FALSE)
  
  data <- reactive({
    csv_path <- "module2_dataset.csv"
    
    if (file.exists(csv_path)) {
      df <- tryCatch(read.csv(csv_path, stringsAsFactors = FALSE), error = function(e) NULL)
      required_cols <- c("Asset_ID", "Resource_Name", "Resource_Type", "Risk_Score",
                         "Risk_Level", "Vulnerability_Count", "Misconfiguration_Count",
                         "Public_Access", "Open_Ports", "Encryption_Status", "IAM_Risk",
                         "Patch_Status", "Logging_Enabled", "Backup_Enabled", "Dependency_Count")
      if (!is.null(df) && all(required_cols %in% colnames(df)) && nrow(df) > 0) {
        data_source_is_sample(FALSE)
        return(df)
      }
    }
    
    # Fallback: generate sample data so the dashboard is never empty,
    # and write it out so the user can inspect / replace it.
    df <- generate_sample_data()
    tryCatch(write.csv(df, csv_path, row.names = FALSE), error = function(e) NULL)
    data_source_is_sample(TRUE)
    df
  })
  
  data_warning_ui <- function() {
    if (isTRUE(data_source_is_sample())) {
      div(class = "data-warning",
          icon("info-circle"),
          " module2_dataset.csv was not found (or was missing required columns), so sample data was generated automatically. Place your real dataset at 'module2_dataset.csv' in the app's working directory to replace it."
      )
    } else {
      NULL
    }
  }
  
  output$data_warning <- renderUI({ data_warning_ui() })
  output$data_warning_summary <- renderUI({ data_warning_ui() })
  
  output$total_assets <- renderText({
    df <- data()
    validate(need(!is.null(df), "No data"))
    nrow(df)
  })
  
  output$misconfig_assets <- renderText({
    df <- data()
    validate(need(!is.null(df), "No data"))
    sum(df$Misconfiguration_Count > 0, na.rm = TRUE)
  })
  
  output$critical_assets <- renderText({
    df <- data()
    validate(need(!is.null(df), "No data"))
    sum(df$Risk_Level == "Critical", na.rm = TRUE)
  })
  
  output$avg_risk <- renderText({
    df <- data()
    validate(need(!is.null(df), "No data"))
    round(mean(df$Risk_Score, na.rm = TRUE), 2)
  })
  
  output$total_vuln <- renderText({
    df <- data()
    validate(need(!is.null(df), "No data"))
    sum(df$Vulnerability_Count, na.rm = TRUE)
  })
  
  output$public_assets <- renderText({
    df <- data()
    validate(need(!is.null(df), "No data"))
    sum(df$Public_Access == "Yes", na.rm = TRUE)
  })
  
  output$risk_dist_chart <- renderPlotly({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    plot_ly(df, x = ~Risk_Score, type = "histogram", nbinsx = 30,
            marker = list(color = "#3498db", line = list(color = "#2980b9", width = 1)),
            hovertemplate = "%{x}: %{y} assets<extra></extra>") %>%
      layout(title = "Risk Score Distribution",
             xaxis = list(title = "Risk Score", gridcolor = "rgba(255,255,255,0.1)"),
             yaxis = list(title = "Count", gridcolor = "rgba(255,255,255,0.1)"),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0.2)",
             font = list(color = "#ecf0f1"))
  })
  
  output$risk_level_pie <- renderPlotly({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    risk_counts <- df %>% group_by(Risk_Level) %>% summarise(Count = n(), .groups = 'drop')
    colors <- c("Critical" = "#e74c3c", "High" = "#f39c12", "Medium" = "#9b59b6", "Low" = "#27ae60")
    
    plot_ly(risk_counts, labels = ~Risk_Level, values = ~Count, type = 'pie',
            marker = list(colors = colors[risk_counts$Risk_Level]),
            textposition = 'inside', textinfo = 'label+percent',
            hovertemplate = "%{label}: %{value}<extra></extra>") %>%
      layout(title = "Risk Level Distribution",
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0.2)",
             font = list(color = "#ecf0f1"))
  })
  
  output$top_risks_table <- DT::renderDataTable({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    top_df <- df %>%
      arrange(desc(Risk_Score)) %>%
      head(10) %>%
      select(Asset_ID, Resource_Name, Risk_Score, Risk_Level, Vulnerability_Count, Misconfiguration_Count)
    
    col_defs <- list()
    idx <- dt_col_index(top_df, "Risk_Level")
    if (!is.null(idx)) {
      col_defs <- list(list(targets = idx, render = risk_badge_renderer()))
    }
    
    datatable(top_df,
              rownames = FALSE,
              extensions = c('Buttons'),
              options = list(
                dom = 'Bfrtip',
                buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                scrollX = TRUE,
                pageLength = 10,
                columnDefs = col_defs
              ),
              escape = FALSE)
  })
  
  output$summary_table <- DT::renderDataTable({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    col_defs <- list()
    idx <- dt_col_index(df, "Risk_Level")
    if (!is.null(idx)) {
      col_defs <- list(list(targets = idx, render = risk_badge_renderer()))
    }
    
    datatable(df,
              rownames = FALSE,
              extensions = c('Buttons'),
              options = list(
                dom = 'Bfrtip',
                buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                scrollX = TRUE,
                pageLength = 10,
                search = list(regex = TRUE),
                columnDefs = col_defs
              ),
              escape = FALSE)
  })
  
  output$findings_table <- DT::renderDataTable({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    findings_df <- df %>%
      select(Asset_ID, Resource_Name, Resource_Type, Public_Access, Open_Ports,
             Encryption_Status, IAM_Risk, Vulnerability_Count, Risk_Level)
    
    col_defs <- list()
    idx <- dt_col_index(findings_df, "Risk_Level")
    if (!is.null(idx)) {
      col_defs <- list(list(targets = idx, render = risk_badge_renderer()))
    }
    
    datatable(findings_df,
              rownames = FALSE,
              extensions = c('Buttons'),
              options = list(
                dom = 'Bfrtip',
                buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                scrollX = TRUE,
                pageLength = 10,
                columnDefs = col_defs
              ),
              escape = FALSE)
  })
  
  output$misconfig_bar <- renderPlotly({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    misconfig_data <- data.frame(
      Category = c("Public Access", "Encryption Disabled", "Outdated Patches",
                   "Logging Disabled", "Backup Disabled"),
      Count = c(
        sum(df$Public_Access == "Yes", na.rm = TRUE),
        sum(df$Encryption_Status == "No", na.rm = TRUE),
        sum(df$Patch_Status == "Outdated", na.rm = TRUE),
        sum(df$Logging_Enabled == "No", na.rm = TRUE),
        sum(df$Backup_Enabled == "No", na.rm = TRUE)
      )
    )
    
    plot_ly(misconfig_data, x = ~Category, y = ~Count, type = 'bar',
            marker = list(color = c("#e74c3c", "#f39c12", "#9b59b6", "#3498db", "#27ae60")),
            hovertemplate = "%{x}: %{y}<extra></extra>") %>%
      layout(title = "Misconfiguration Frequency",
             xaxis = list(title = "", gridcolor = "rgba(255,255,255,0.1)"),
             yaxis = list(title = "Count", gridcolor = "rgba(255,255,255,0.1)"),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0.2)",
             font = list(color = "#ecf0f1"))
  })
  
  output$misconfig_pie <- renderPlotly({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    misconfig_data <- data.frame(
      Category = c("Public Access", "Encryption Disabled", "Outdated Patches",
                   "Logging Disabled", "Backup Disabled"),
      Count = c(
        sum(df$Public_Access == "Yes", na.rm = TRUE),
        sum(df$Encryption_Status == "No", na.rm = TRUE),
        sum(df$Patch_Status == "Outdated", na.rm = TRUE),
        sum(df$Logging_Enabled == "No", na.rm = TRUE),
        sum(df$Backup_Enabled == "No", na.rm = TRUE)
      )
    )
    
    plot_ly(misconfig_data, labels = ~Category, values = ~Count, type = 'pie',
            marker = list(colors = c("#e74c3c", "#f39c12", "#9b59b6", "#3498db", "#27ae60")),
            hovertemplate = "%{label}: %{value}<extra></extra>") %>%
      layout(title = "Misconfiguration Distribution",
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0.2)",
             font = list(color = "#ecf0f1"))
  })
  
  output$risk_comparison <- renderPlotly({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    plot_ly(df, x = ~Misconfiguration_Count, y = ~Risk_Score,
            color = ~Risk_Level, type = 'scatter', mode = 'markers',
            marker = list(size = 8, opacity = 0.6),
            colors = c("Critical" = "#e74c3c", "High" = "#f39c12", "Medium" = "#9b59b6", "Low" = "#27ae60"),
            hovertemplate = "Misconfigs: %{x}<br>Risk: %{y}<extra></extra>") %>%
      layout(title = "Risk vs Misconfiguration Count",
             xaxis = list(title = "Misconfiguration Count", gridcolor = "rgba(255,255,255,0.1)"),
             yaxis = list(title = "Risk Score", gridcolor = "rgba(255,255,255,0.1)"),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0.2)",
             font = list(color = "#ecf0f1"))
  })
  
  output$misconfig_donut <- renderPlotly({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    risk_counts <- df %>% group_by(Risk_Level) %>% summarise(Count = n(), .groups = 'drop')
    colors <- c("Critical" = "#e74c3c", "High" = "#f39c12", "Medium" = "#9b59b6", "Low" = "#27ae60")
    
    plot_ly(risk_counts, labels = ~Risk_Level, values = ~Count, type = 'pie',
            marker = list(colors = colors[risk_counts$Risk_Level],
                          line = list(color = '#1a2e4a', width = 2)),
            hole = 0.4,
            hovertemplate = "%{label}: %{value}<extra></extra>") %>%
      layout(title = "Risk Level Donut Chart",
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0.2)",
             font = list(color = "#ecf0f1"))
  })
  
  output$risk_histogram <- renderPlotly({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    plot_ly(df, x = ~Risk_Score, type = "histogram", nbinsx = 40,
            marker = list(color = "#3498db", line = list(color = "#2980b9", width = 1)),
            hovertemplate = "Risk: %{x}<br>Count: %{y}<extra></extra>") %>%
      layout(title = "Risk Score Distribution",
             xaxis = list(title = "Risk Score", gridcolor = "rgba(255,255,255,0.1)"),
             yaxis = list(title = "Frequency", gridcolor = "rgba(255,255,255,0.1)"),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0.2)",
             font = list(color = "#ecf0f1"))
  })
  
  output$risk_boxplot <- renderPlotly({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    plot_ly(data = df, x = ~Resource_Type, y = ~Risk_Score, type = "box",
            marker = list(color = "#9b59b6"), line = list(color = "#9b59b6"),
            hovertemplate = "%{y}<extra></extra>") %>%
      layout(title = "Risk Distribution by Resource Type",
             xaxis = list(title = "Resource Type", gridcolor = "rgba(255,255,255,0.1)"),
             yaxis = list(title = "Risk Score", gridcolor = "rgba(255,255,255,0.1)"),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0.2)",
             font = list(color = "#ecf0f1"))
  })
  
  output$risk_scatter <- renderPlotly({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    plot_ly(df, x = ~Vulnerability_Count, y = ~Risk_Score,
            color = ~Risk_Level, type = 'scatter', mode = 'markers',
            marker = list(size = 8, opacity = 0.6),
            colors = c("Critical" = "#e74c3c", "High" = "#f39c12", "Medium" = "#9b59b6", "Low" = "#27ae60"),
            hovertemplate = "Vuln: %{x}<br>Risk: %{y}<extra></extra>") %>%
      layout(title = "Risk Score vs Vulnerability Count",
             xaxis = list(title = "Vulnerability Count", gridcolor = "rgba(255,255,255,0.1)"),
             yaxis = list(title = "Risk Score", gridcolor = "rgba(255,255,255,0.1)"),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0.2)",
             font = list(color = "#ecf0f1"))
  })
  
  output$correlation_heatmap <- renderPlotly({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    # FIX: drop Asset_ID (and any other ID-like text column) BEFORE
    # filtering to numeric columns, so select() never references a
    # column that select_if() has already removed.
    numeric_cols <- df %>%
      select(-any_of(c("Asset_ID"))) %>%
      select_if(is.numeric)
    
    validate(need(ncol(numeric_cols) >= 2, "Not enough numeric columns to compute a correlation matrix"))
    
    cor_matrix <- cor(numeric_cols, use = "complete.obs")
    
    plot_ly(z = cor_matrix, type = "heatmap", colorscale = "Viridis",
            x = colnames(cor_matrix), y = rownames(cor_matrix),
            hovertemplate = "%{x} - %{y}: %{z:.2f}<extra></extra>") %>%
      layout(title = "Feature Correlation Matrix",
             xaxis = list(tickangle = -45),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0.2)",
             font = list(color = "#ecf0f1"),
             margin = list(b = 100))
  })
  
  output$kmeans_cluster <- renderPlotly({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    set.seed(42)
    
    # Same fix as correlation_heatmap: drop ID column first.
    numeric_df <- df %>%
      select(-any_of(c("Asset_ID"))) %>%
      select_if(is.numeric)
    
    validate(need(ncol(numeric_df) >= 2 && nrow(numeric_df) >= 3,
                  "Not enough numeric data to compute clusters"))
    
    numeric_df_scaled <- scale(numeric_df)
    numeric_df_scaled[is.nan(numeric_df_scaled)] <- 0
    
    km <- kmeans(numeric_df_scaled, centers = 3, nstart = 10)
    
    plot_ly(df, x = ~Risk_Score, y = ~Vulnerability_Count,
            color = factor(km$cluster), type = 'scatter', mode = 'markers',
            marker = list(size = 8, opacity = 0.7),
            colors = c("#3498db", "#e74c3c", "#27ae60"),
            hovertemplate = "Risk: %{x}<br>Vuln: %{y}<extra></extra>") %>%
      layout(title = "KMeans Clustering (K=3)",
             xaxis = list(title = "Risk Score", gridcolor = "rgba(255,255,255,0.1)"),
             yaxis = list(title = "Vulnerability Count", gridcolor = "rgba(255,255,255,0.1)"),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0.2)",
             font = list(color = "#ecf0f1"),
             showlegend = FALSE)
  })
  
  output$ports_vs_risk <- renderPlotly({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    plot_ly(df, x = ~Open_Ports, y = ~Risk_Score,
            type = 'scatter', mode = 'markers',
            marker = list(size = 8, color = "#f39c12", opacity = 0.6),
            hovertemplate = "Ports: %{x}<br>Risk: %{y}<extra></extra>") %>%
      layout(title = "Open Ports vs Risk Score",
             xaxis = list(title = "Open Ports", gridcolor = "rgba(255,255,255,0.1)"),
             yaxis = list(title = "Risk Score", gridcolor = "rgba(255,255,255,0.1)"),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0.2)",
             font = list(color = "#ecf0f1"))
  })
  
  output$dependency_analysis <- renderPlotly({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    dep_risk <- df %>%
      group_by(Dependency_Count) %>%
      summarise(Avg_Risk = mean(Risk_Score, na.rm = TRUE), .groups = 'drop') %>%
      arrange(Dependency_Count)
    
    plot_ly(dep_risk, x = ~Dependency_Count, y = ~Avg_Risk, type = 'scatter',
            mode = 'lines+markers', line = list(color = "#9b59b6", width = 3),
            marker = list(size = 8, color = "#9b59b6"),
            fill = 'tozeroy', fillcolor = "rgba(155, 89, 182, 0.2)",
            hovertemplate = "Dependencies: %{x}<br>Avg Risk: %{y:.1f}<extra></extra>") %>%
      layout(title = "Dependency Count vs Average Risk",
             xaxis = list(title = "Dependency Count", gridcolor = "rgba(255,255,255,0.1)"),
             yaxis = list(title = "Average Risk Score", gridcolor = "rgba(255,255,255,0.1)"),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0.2)",
             font = list(color = "#ecf0f1"))
  })
  
  output$recommendations_ui <- renderUI({
    df <- data()
    validate(need(!is.null(df) && nrow(df) > 0, "No data available"))
    
    recommendations <- list()
    
    for (i in seq_len(nrow(df))) {
      asset <- df[i, ]
      rec_list <- list()
      
      if (isTRUE(asset$Public_Access == "Yes")) {
        rec_list[[length(rec_list) + 1]] <- HTML(
          sprintf("<li style='color: #e74c3c;'><strong>Asset: %s</strong> - Disable public access immediately</li>",
                  asset$Resource_Name)
        )
      }
      
      if (isTRUE(asset$Encryption_Status == "No")) {
        rec_list[[length(rec_list) + 1]] <- HTML(
          sprintf("<li style='color: #f39c12;'><strong>Asset: %s</strong> - Enable encryption</li>",
                  asset$Resource_Name)
        )
      }
      
      if (isTRUE(asset$Patch_Status == "Outdated")) {
        rec_list[[length(rec_list) + 1]] <- HTML(
          sprintf("<li style='color: #f39c12;'><strong>Asset: %s</strong> - Apply security patches</li>",
                  asset$Resource_Name)
        )
      }
      
      if (isTRUE(asset$Logging_Enabled == "No")) {
        rec_list[[length(rec_list) + 1]] <- HTML(
          sprintf("<li style='color: #9b59b6;'><strong>Asset: %s</strong> - Enable cloud logging</li>",
                  asset$Resource_Name)
        )
      }
      
      if (isTRUE(asset$Backup_Enabled == "No")) {
        rec_list[[length(rec_list) + 1]] <- HTML(
          sprintf("<li style='color: #9b59b6;'><strong>Asset: %s</strong> - Enable backup</li>",
                  asset$Resource_Name)
        )
      }
      
      if (isTRUE(asset$Risk_Score > 75)) {
        rec_list[[length(rec_list) + 1]] <- HTML(
          sprintf("<li style='color: #e74c3c;'><strong>CRITICAL ALERT - Asset: %s</strong> - Risk Score: %s (&gt;75)</li>",
                  asset$Resource_Name, round(asset$Risk_Score, 2))
        )
      }
      
      if (length(rec_list) > 0) {
        recommendations[[length(recommendations) + 1]] <- HTML(
          sprintf("<div style='background: rgba(52, 152, 219, 0.1); padding: 10px; margin: 5px 0; border-left: 4px solid #3498db;'>
                    <ul style='margin:0; padding-left: 18px;'>%s</ul>
                  </div>", paste(unlist(rec_list), collapse = ""))
        )
      }
    }
    
    if (length(recommendations) == 0) {
      return(HTML("<p style='color: #27ae60;'>All assets are properly configured!</p>"))
    }
    
    tagList(
      do.call(tagList, recommendations),
      HTML(sprintf("<p style='margin-top: 20px; color: #bdc3c7;'><strong>Total Recommendations: %d</strong></p>",
                   length(recommendations)))
    )
  })
  
  output$arch_diagram <- renderPlotly({
    steps <- c(
      "Cloud Infrastructure (AWS)",
      "IaC / Config Parser",
      "Misconfiguration Detection Engine",
      "Risk Scoring Engine",
      "Blast Radius Analysis",
      "Recommendation Generator",
      "Interactive Dashboard"
    )
    build_flow_diagram(
      steps = steps,
      box_color = "#3498db",
      fill_rgba = "rgba(52, 152, 219, 0.15)",
      title_text = "IaCGuardian Architecture Flow"
    )
  })
  
  output$flow_diagram <- renderPlotly({
    steps <- c(
      "Upload Dataset",
      "Validate Schema & Data",
      "Detect Misconfigurations",
      "Calculate Risk Score",
      "Classify Risk Level",
      "Generate Recommendations",
      "Visualize Results"
    )
    build_flow_diagram(
      steps = steps,
      box_color = "#27ae60",
      fill_rgba = "rgba(39, 174, 96, 0.15)",
      title_text = "Misconfiguration Detection Flow"
    )
  })
}

shinyApp(ui, server)