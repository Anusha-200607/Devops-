# ============================================================
# CO3 - ASSESSMENT TASK 1
# Simulation and Modelling: SAST
# Static Application Security Testing using Synthetic Data
# ============================================================
#
# Run:
#   install.packages(c("shiny","ggplot2","DT","DiagrammeR","dplyr","tidyr"))
#   shiny::runApp("C03_AT_1.R")
#
# The program:
# 1. Generates a synthetic source-code vulnerability dataset.
# 2. Simulates SAST scanning and risk scoring.
# 3. Displays an editable Shiny dashboard.
# 4. Shows architecture, simulation tables, charts and security results.
# ============================================================

required <- c("shiny","ggplot2","DT","DiagrammeR","dplyr","tidyr")
missing <- required[!sapply(required, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  stop(paste0("Install required packages first: ", paste(missing, collapse = ", ")))
}

library(shiny)
library(ggplot2)
library(DT)
library(DiagrammeR)
library(dplyr)
library(tidyr)

set.seed(2026)

# ------------------------- SYNTHETIC DATA ---------------------
make_sast_data <- function(n = 180) {
  vuln_types <- c(
    "SQL Injection", "XSS", "Hardcoded Secret", "Buffer Overflow",
    "Insecure Deserialization", "Command Injection", "Path Traversal",
    "Weak Cryptography", "Missing Input Validation", "Insecure API"
  )
  severity <- c("Critical", "High", "Medium", "Low")
  languages <- c("Python","Java","C++","JavaScript","PHP")
  modules <- c("Authentication","Payments","User API","Admin Portal",
               "Database Layer","File Service","Logging","Reporting")
  
  data.frame(
    Scan_ID = sprintf("SAST-%03d", 1:n),
    Module = sample(modules, n, replace = TRUE),
    Language = sample(languages, n, replace = TRUE),
    Vulnerability = sample(vuln_types, n, replace = TRUE),
    Severity = sample(severity, n, replace = TRUE, prob = c(.12,.28,.40,.20)),
    CVSS = round(pmin(10, pmax(1, rnorm(n, 6.4, 2.0))), 1),
    Lines_Changed = sample(5:350, n, replace = TRUE),
    Detection_Probability = round(runif(n, .72, .99), 2),
    stringsAsFactors = FALSE
  ) |>
    mutate(
      SAST_Detected = rbinom(n(), 1, Detection_Probability),
      Remediation_Days = case_when(
        Severity == "Critical" ~ sample(1:5, n(), replace = TRUE),
        Severity == "High" ~ sample(3:10, n(), replace = TRUE),
        Severity == "Medium" ~ sample(7:20, n(), replace = TRUE),
        TRUE ~ sample(10:30, n(), replace = TRUE)
      ),
      Risk_Score = round(CVSS * (1 + ifelse(Severity=="Critical", .35,
                                            ifelse(Severity=="High", .20,
                                                   ifelse(Severity=="Medium", .10, 0)))), 2)
    )
}

base_data <- make_sast_data()

# ------------------------- UI STYLE ----------------------------
app_css <- "
body { background:#f4f7fb; font-family:Cambria, Georgia, serif; }
h2,h3 { font-family:Cambria, Georgia, serif; font-weight:700; }
.navbar { background:#182848 !important; }
.navbar-brand { color:white !important; font-weight:700; font-size:22px; }
.navbar-nav > li > a { color:white !important; font-weight:700; }
.panel-title { font-weight:700; }
.well { background:white; border:1px solid #d7dfeb; border-radius:12px; }
.value-box { border-radius:14px; }
.dataTables_wrapper { font-family:Cambria, Georgia, serif; }
table.dataTable thead th { background:#243b64; color:white; font-weight:700; }
.info-box { background:white; border-radius:14px; padding:18px; margin-bottom:18px;
            box-shadow:0 2px 8px rgba(0,0,0,.08); }
"

# ------------------------- SERVER -----------------------------
server <- function(input, output, session) {
  filtered <- reactive({
    d <- base_data
    if (!is.null(input$severity) && input$severity != "All")
      d <- d[d$Severity == input$severity, ]
    if (!is.null(input$language) && input$language != "All")
      d <- d[d$Language == input$language, ]
    d
  })
  
  output$kpi_total <- renderText(nrow(filtered()))
  output$kpi_detected <- renderText(sum(filtered()$SAST_Detected))
  output$kpi_critical <- renderText(sum(filtered()$Severity == "Critical"))
  output$kpi_detection <- renderText(
    paste0(round(mean(filtered()$SAST_Detected)*100,1), "%")
  )
  
  output$arch <- renderGrViz({
    grViz("
      digraph sast {
        graph [rankdir=LR, bgcolor='transparent', nodesep=.35, ranksep=.55]
        node [shape=box, style='rounded,filled', fontname='Cambria Bold',
              fontsize=18, margin='.20,.12', color='#203864', penwidth=2]
        edge [color='#667085', penwidth=2, arrowsize=.7]
        A [label='SOURCE CODE', fillcolor='#DDEBF7']
        B [label='BUILD / CHECKOUT', fillcolor='#E2F0D9']
        C [label='SAST SCANNER', fillcolor='#FFF2CC']
        D [label='VULNERABILITY\\nCLASSIFICATION', fillcolor='#FCE4D6']
        E [label='RISK / CVSS\\nSCORING', fillcolor='#E4DFEC']
        F [label='REMEDIATION\\nDECISION', fillcolor='#D9EAD3']
        A -> B -> C -> D -> E -> F
      }
    ")
  })
  
  output$severity_plot <- renderPlot({
    d <- filtered() |> count(Severity)
    ggplot(d, aes(x=reorder(Severity, -n), y=n, fill=Severity)) +
      geom_col(width=.68) +
      geom_text(aes(label=n), vjust=-.35, fontface="bold", size=5) +
      scale_fill_manual(values=c("Critical"="#C00000","High"="#ED7D31",
                                 "Medium"="#FFC000","Low"="#70AD47")) +
      labs(title="SAST Vulnerability Severity Distribution",
           x=NULL, y="Number of Vulnerabilities") +
      theme_minimal(base_family="Cambria", base_size=14) +
      theme(legend.position="none", plot.title=element_text(face="bold"))
  })
  
  output$risk_plot <- renderPlot({
    ggplot(filtered(), aes(x=CVSS, y=Risk_Score, color=Severity)) +
      geom_point(size=3.2, alpha=.75) +
      geom_smooth(method="lm", se=FALSE, linewidth=1) +
      scale_color_manual(values=c("Critical"="#C00000","High"="#ED7D31",
                                  "Medium"="#FFC000","Low"="#70AD47")) +
      labs(title="CVSS vs Calculated Risk Score",
           x="CVSS Score", y="Risk Score") +
      theme_minimal(base_family="Cambria", base_size=14) +
      theme(plot.title=element_text(face="bold"))
  })
  
  output$table <- renderDT({
    datatable(
      filtered() |> mutate(SAST_Detected=ifelse(SAST_Detected==1,"YES","NO")),
      rownames=FALSE,
      options=list(pageLength=12, scrollX=TRUE, autoWidth=TRUE),
      class="stripe hover"
    )
  })
  
  output$summary <- renderDT({
    filtered() |>
      group_by(Severity) |>
      summarise(
        Vulnerabilities=n(),
        Detected=sum(SAST_Detected),
        Detection_Rate=paste0(round(mean(SAST_Detected)*100,1),"%"),
        Mean_CVSS=round(mean(CVSS),2),
        Mean_Risk=round(mean(Risk_Score),2),
        .groups="drop"
      ) |>
      datatable(rownames=FALSE, options=list(dom="t", pageLength=10))
  })
}

# --------------------------- UI -------------------------------
ui <- navbarPage(
  title="CO3 • AT-1 • SAST Simulation",
  header=tags$head(tags$style(HTML(app_css))),
  
  tabPanel("Dashboard",
           fluidRow(
             column(3, div(class="info-box", h4("Total Findings"), h2(textOutput("kpi_total")))),
             column(3, div(class="info-box", h4("Detected by SAST"), h2(textOutput("kpi_detected")))),
             column(3, div(class="info-box", h4("Critical Findings"), h2(textOutput("kpi_critical")))),
             column(3, div(class="info-box", h4("Detection Rate"), h2(textOutput("kpi_detection"))))
           ),
           fluidRow(
             column(3,
                    wellPanel(
                      h4("Simulation Controls"),
                      selectInput("severity","Severity",c("All","Critical","High","Medium","Low")),
                      selectInput("language","Language",c("All",sort(unique(base_data$Language))))
                    )
             ),
             column(9, div(class="info-box", h3("SAST Architecture"), grVizOutput("arch", height="190px")))
           ),
           fluidRow(
             column(6, div(class="info-box", plotOutput("severity_plot", height="360px"))),
             column(6, div(class="info-box", plotOutput("risk_plot", height="360px")))
           )
  ),
  
  tabPanel("Simulation Table",
           div(class="info-box",
               h3("Synthetic SAST Vulnerability Dataset"),
               p("The table represents simulated source-code findings generated for the assessment."),
               DTOutput("table"))
  ),
  
  tabPanel("Security Analysis",
           div(class="info-box",
               h3("Severity-wise Security Verification Summary"),
               DTOutput("summary")),
           div(class="info-box",
               h3("Interpretation"),
               p("Critical and High findings receive larger risk multipliers. The dashboard models how SAST
          can identify vulnerabilities before deployment and supports risk-based remediation decisions."))
  ),
  
  tabPanel("About",
           div(class="info-box",
               h3("CO3 - Assessment No.1"),
               p(strong("Assessment Type: "), "Simulation and Modelling"),
               p(strong("Activity: "), "Students simulate Static Application Security Testing (SAST) using synthetic
          datasets and analyze security vulnerabilities."),
               h4("Expected Outcome"),
               tags$ul(
                 tags$li("Generate synthetic vulnerability observations."),
                 tags$li("Simulate SAST detection probability."),
                 tags$li("Calculate CVSS-based risk."),
                 tags$li("Compare severity and detection performance."),
                 tags$li("Use the Shiny dashboard for visual security analysis.")
               ))
  )
)

shinyApp(ui, server)
