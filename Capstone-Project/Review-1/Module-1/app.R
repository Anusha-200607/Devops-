# ==========================================================
# IaCGuardian - Module 2
# Intelligent Misconfiguration Detection & Risk Scoring
# ==========================================================

# ==========================================================
# 1. REQUIRED PACKAGES
# ==========================================================
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)

packages <- c(
  "shiny",
  "shinydashboard",
  "DT",
  "ggplot2",
  "dplyr"
)

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}


# ==========================================================
# 2. DATASET
# ==========================================================

dataset_file <- "iac_dataset.csv"


# ----------------------------------------------------------
# Create Sample Dataset if File Does Not Exist
# ----------------------------------------------------------

if (!file.exists(dataset_file)) {
  
  set.seed(123)
  
  n <- 50
  
  iac_data <- data.frame(
    
    Asset_ID = paste0(
      "ASSET-",
      sprintf("%03d", 1:n)
    ),
    
    Resource_Type = sample(
      c(
        "EC2",
        "RDS",
        "S3",
        "IAM",
        "VPC",
        "Lambda"
      ),
      n,
      replace = TRUE
    ),
    
    Resource_Name = paste(
      "Cloud-Resource",
      1:n
    ),
    
    Open_Ports = sample(
      0:15,
      n,
      replace = TRUE
    ),
    
    Risk_Score = sample(
      10:100,
      n,
      replace = TRUE
    ),
    
    Criticality = sample(
      c(
        "Low",
        "Medium",
        "High",
        "Critical"
      ),
      n,
      replace = TRUE
    ),
    
    Dependency_Count = sample(
      0:12,
      n,
      replace = TRUE
    ),
    
    Public_Access = sample(
      c(
        "Yes",
        "No"
      ),
      n,
      replace = TRUE
    ),
    
    IAM_Risk = sample(
      c(
        "Low",
        "Medium",
        "High"
      ),
      n,
      replace = TRUE
    ),
    
    stringsAsFactors = FALSE
    
  )
  
  write.csv(
    iac_data,
    dataset_file,
    row.names = FALSE
  )
  
}


# ==========================================================
# 3. READ DATA
# ==========================================================

data <- read.csv(
  dataset_file,
  stringsAsFactors = FALSE
)


# ==========================================================
# 4. CREATE MISCONFIGURATION DETECTION LOGIC
# ==========================================================

data <- data %>%
  
  mutate(
    
    # Public Exposure Detection
    Public_Exposure = ifelse(
      Public_Access == "Yes",
      "Detected",
      "Secure"
    ),
    
    # Open Port Detection
    Open_Port_Risk = ifelse(
      Open_Ports >= 5,
      "Detected",
      "Secure"
    ),
    
    # IAM Misconfiguration Detection
    IAM_Misconfiguration = ifelse(
      IAM_Risk == "High",
      "Detected",
      "Secure"
    ),
    
    # Dependency Risk Detection
    Dependency_Risk = ifelse(
      Dependency_Count >= 8,
      "Detected",
      "Secure"
    )
    
  )


# ==========================================================
# 5. GENERATE MISCONFIGURATION TYPE
# ==========================================================

data$Misconfiguration_Type <- apply(
  
  data,
  
  1,
  
  function(row) {
    
    issues <- c()
    
    if (row["Public_Access"] == "Yes") {
      issues <- c(issues, "Public Exposure")
    }
    
    if (as.numeric(row["Open_Ports"]) >= 5) {
      issues <- c(issues, "Open Ports")
    }
    
    if (row["IAM_Risk"] == "High") {
      issues <- c(issues, "Excessive IAM Permission")
    }
    
    if (as.numeric(row["Dependency_Count"]) >= 8) {
      issues <- c(issues, "High Dependency Risk")
    }
    
    if (length(issues) == 0) {
      return("No Misconfiguration")
    }
    
    paste(issues, collapse = ", ")
    
  }
  
)


# ==========================================================
# 6. CALCULATE INTELLIGENT RISK SCORE
# ==========================================================

data <- data %>%
  
  mutate(
    
    Exposure_Score = ifelse(
      Public_Access == "Yes",
      25,
      0
    ),
    
    Port_Score = pmin(
      Open_Ports * 2,
      20
    ),
    
    IAM_Score = case_when(
      IAM_Risk == "High" ~ 25,
      IAM_Risk == "Medium" ~ 15,
      TRUE ~ 5
    ),
    
    Dependency_Score = pmin(
      Dependency_Count * 2,
      20
    ),
    
    Criticality_Score = case_when(
      Criticality == "Critical" ~ 10,
      Criticality == "High" ~ 8,
      Criticality == "Medium" ~ 5,
      TRUE ~ 2
    ),
    
    Calculated_Risk = pmin(
      100,
      Exposure_Score +
        Port_Score +
        IAM_Score +
        Dependency_Score +
        Criticality_Score
    )
    
  )


# ==========================================================
# 7. RISK LEVEL
# ==========================================================

data <- data %>%
  
  mutate(
    
    Risk_Level = case_when(
      Calculated_Risk >= 80 ~ "Critical",
      Calculated_Risk >= 60 ~ "High",
      Calculated_Risk >= 40 ~ "Medium",
      TRUE ~ "Low"
    )
    
  )


# ==========================================================
# 8. RECOMMENDATIONS
# ==========================================================

data <- data %>%
  
  mutate(
    
    Recommendation = case_when(
      
      Public_Access == "Yes" &
        IAM_Risk == "High" ~
        "Restrict public access and apply least-privilege IAM permissions.",
      
      Public_Access == "Yes" ~
        "Restrict public access to trusted networks or authorized users.",
      
      IAM_Risk == "High" ~
        "Review IAM permissions and apply the principle of least privilege.",
      
      Open_Ports >= 5 ~
        "Close unnecessary ports and restrict network access.",
      
      Dependency_Count >= 8 ~
        "Review asset dependencies and isolate critical resources.",
      
      TRUE ~
        "No immediate action required."
      
    )
    
  )


# ==========================================================
# 9. USER INTERFACE
# ==========================================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = span(
      icon("shield-halved"),
      " IaCGuardian - Module 2"
    )
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Misconfiguration Dashboard",
        tabName = "dashboard",
        icon = icon("gauge-high")
      ),
      
      menuItem(
        "Security Findings",
        tabName = "findings",
        icon = icon("triangle-exclamation")
      ),
      
      menuItem(
        "Risk Analytics",
        tabName = "analytics",
        icon = icon("chart-line")
      ),
      
      menuItem(
        "Recommendations",
        tabName = "recommendations",
        icon = icon("lightbulb")
      ),
      
      menuItem(
        "Summary Report",
        tabName = "summary",
        icon = icon("table-list")
      )
      
    ),
    
    br(),
    
    div(
      style = "padding:15px;",
      
      selectInput(
        "rtype",
        "Filter Resource Type",
        choices = c("All", sort(unique(data$Resource_Type))),
        selected = "All"
      ),
      
      selectInput(
        "risk_filter",
        "Filter Risk Level",
        choices = c("All", "Critical", "High", "Medium", "Low"),
        selected = "All"
      )
    )
    
  ),
  
  dashboardBody(
    
    tags$head(
      
      tags$style(
        HTML(
          "
          .content-wrapper,
          .right-side {
            background-color: #eef3f8;
          }

          .main-header .logo {
            background-color: #101827 !important;
            font-size: 21px;
            font-weight: bold;
          }

          .main-header .navbar {
            background-color: #101827 !important;
          }

          .main-sidebar {
            background-color: #111827 !important;
          }

          .small-box {
            border-radius: 15px !important;
            min-height: 145px !important;
            box-shadow: 0px 5px 18px rgba(0,0,0,0.12);
            transition: transform 0.2s;
          }

          .small-box:hover {
            transform: translateY(-5px);
          }

          .small-box h3 {
            font-size: 36px !important;
            font-weight: bold;
          }

          .small-box p {
            font-size: 16px !important;
          }

          .box {
            border-radius: 14px !important;
            box-shadow: 0px 4px 15px rgba(0,0,0,0.08);
          }

          .box-title {
            font-size: 17px !important;
            font-weight: bold;
          }

          .dashboard-title {
            font-size: 24px;
            font-weight: bold;
            color: #172033;
            margin-bottom: 20px;
          }

          table.dataTable {
            font-size: 14px;
          }

          table.dataTable thead th {
            background-color: #172033 !important;
            color: white !important;
          }

          table.dataTable tfoot th {
            background-color: #eef3f8 !important;
            font-weight: bold;
          }
          "
        )
      )
    ),
    
    tabItems(
      
      # ====================================================
      # TAB 1 - DASHBOARD
      # ====================================================
      
      tabItem(
        tabName = "dashboard",
        
        h2(
          "Intelligent Misconfiguration Detection",
          class = "dashboard-title"
        ),
        
        # KPI ROW 1
        fluidRow(
          valueBoxOutput("total_issues", width = 4),
          valueBoxOutput("critical_issues", width = 4),
          valueBoxOutput("high_issues", width = 4)
        ),
        
        # KPI ROW 2
        fluidRow(
          valueBoxOutput("avg_risk", width = 4),
          valueBoxOutput("public_issues", width = 4),
          valueBoxOutput("iam_issues", width = 4)
        ),
        
        # RISK DISTRIBUTION
        fluidRow(
          box(
            width = 6,
            title = "Risk Severity Distribution",
            status = "danger",
            solidHeader = TRUE,
            plotOutput("risk_distribution", height = "400px")
          ),
          
          box(
            width = 6,
            title = "Misconfiguration Types",
            status = "warning",
            solidHeader = TRUE,
            plotOutput("misconfiguration_chart", height = "400px")
          )
        )
      ),
      
      # ====================================================
      # TAB 2 - SECURITY FINDINGS
      # ====================================================
      
      tabItem(
        tabName = "findings",
        
        h2(
          "Security Misconfiguration Findings",
          class = "dashboard-title"
        ),
        
        box(
          width = 12,
          title = tagList(icon("bug"), " Detected Security Issues"),
          status = "danger",
          solidHeader = TRUE,
          DTOutput("findings_table")
        )
      ),
      
      # ====================================================
      # TAB 3 - RISK ANALYTICS
      # ====================================================
      
      tabItem(
        tabName = "analytics",
        
        h2(
          "Intelligent Risk Analytics",
          class = "dashboard-title"
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Risk Score Distribution",
            status = "warning",
            solidHeader = TRUE,
            plotOutput("risk_hist", height = "400px")
          ),
          
          box(
            width = 6,
            title = "Risk vs Infrastructure Exposure",
            status = "danger",
            solidHeader = TRUE,
            plotOutput("risk_scatter", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Risk Score Components",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("risk_components", height = "450px")
          )
        )
      ),
      
      # ====================================================
      # TAB 4 - RECOMMENDATIONS
      # ====================================================
      
      tabItem(
        tabName = "recommendations",
        
        h2(
          "Security Recommendation Engine",
          class = "dashboard-title"
        ),
        
        box(
          width = 12,
          title = tagList(icon("lightbulb"), " Prioritized Security Recommendations"),
          status = "success",
          solidHeader = TRUE,
          DTOutput("recommendation_table")
        )
      ),
      
      # ====================================================
      # TAB 5 - SUMMARY REPORT
      # ====================================================
      
      tabItem(
        tabName = "summary",
        
        h2(
          "Executive Summary Report",
          class = "dashboard-title"
        ),
        
        fluidRow(
          valueBoxOutput("summary_total_assets", width = 3),
          valueBoxOutput("summary_total_misconfig", width = 3),
          valueBoxOutput("summary_pct_misconfig", width = 3),
          valueBoxOutput("summary_avg_risk", width = 3)
        ),
        
        box(
          width = 12,
          title = tagList(icon("table-list"), " Summary by Resource Type"),
          status = "primary",
          solidHeader = TRUE,
          DTOutput("summary_table")
        )
      )
      
    )
  )
)


# ==========================================================
# 10. SERVER
# ==========================================================

server <- function(input, output, session) {
  
  # Filter Data
  filtered_data <- reactive({
    
    result <- data
    
    if (input$rtype != "All") {
      result <- result %>%
        filter(Resource_Type == input$rtype)
    }
    
    if (input$risk_filter != "All") {
      result <- result %>%
        filter(Risk_Level == input$risk_filter)
    }
    
    result
  })
  
  
  # Total Misconfigurations
  output$total_issues <- renderValueBox({
    
    issues <- sum(
      filtered_data()$Misconfiguration_Type != "No Misconfiguration"
    )
    
    valueBox(
      value = issues,
      subtitle = "Detected Misconfigurations",
      icon = icon("bug"),
      color = "red"
    )
  })
  
  
  # Critical Issues
  output$critical_issues <- renderValueBox({
    valueBox(
      value = sum(filtered_data()$Risk_Level == "Critical"),
      subtitle = "Critical Risk Issues",
      icon = icon("skull-crossbones"),
      color = "purple"
    )
  })
  
  
  # High Issues
  output$high_issues <- renderValueBox({
    valueBox(
      value = sum(filtered_data()$Risk_Level == "High"),
      subtitle = "High Risk Issues",
      icon = icon("triangle-exclamation"),
      color = "red"
    )
  })
  
  
  # Average Risk
  output$avg_risk <- renderValueBox({
    valueBox(
      value = round(mean(filtered_data()$Calculated_Risk, na.rm = TRUE), 1),
      subtitle = "Average Calculated Risk",
      icon = icon("chart-line"),
      color = "orange"
    )
  })
  
  
  # Public Exposure
  output$public_issues <- renderValueBox({
    valueBox(
      value = sum(filtered_data()$Public_Access == "Yes"),
      subtitle = "Public Exposure Issues",
      icon = icon("globe"),
      color = "yellow"
    )
  })
  
  
  # IAM Issues
  output$iam_issues <- renderValueBox({
    valueBox(
      value = sum(filtered_data()$IAM_Risk == "High"),
      subtitle = "High IAM Risk Issues",
      icon = icon("key"),
      color = "blue"
    )
  })
  
  
  # Findings Table
  output$findings_table <- renderDT({
    
    result <- filtered_data() %>%
      filter(Misconfiguration_Type != "No Misconfiguration") %>%
      select(
        Asset_ID,
        Resource_Name,
        Resource_Type,
        Misconfiguration_Type,
        Calculated_Risk,
        Risk_Level,
        Public_Access,
        IAM_Risk,
        Open_Ports,
        Dependency_Count
      ) %>%
      arrange(desc(Calculated_Risk))
    
    datatable(
      result,
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "print"),
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Calculated_Risk",
        backgroundColor = styleInterval(
          c(40, 60, 80),
          c("#28a745", "#f39c12", "#dc3545", "#8b0000")
        ),
        color = "white",
        fontWeight = "bold"
      )
  })
  
  
  # Risk Distribution
  output$risk_distribution <- renderPlot({
    
    counts <- table(filtered_data()$Risk_Level)
    
    barplot(
      counts,
      col = c("#8b0000", "#dc3545", "#f39c12", "#28a745"),
      main = "Security Risk Severity",
      xlab = "Risk Level",
      ylab = "Number of Assets"
    )
  })
  
  
  # Misconfiguration Chart
  output$misconfiguration_chart <- renderPlot({
    
    issues <- filtered_data() %>%
      filter(Misconfiguration_Type != "No Misconfiguration") %>%
      count(Misconfiguration_Type) %>%
      arrange(desc(n))
    
    barplot(
      issues$n,
      names.arg = issues$Misconfiguration_Type,
      las = 2,
      col = rainbow(nrow(issues)),
      main = "Detected Misconfiguration Categories",
      ylab = "Number of Occurrences"
    )
  })
  
  
  # Risk Histogram
  output$risk_hist <- renderPlot({
    
    ggplot(
      filtered_data(),
      aes(x = Calculated_Risk, fill = Risk_Level)
    ) +
      geom_histogram(binwidth = 10, color = "white") +
      labs(
        title = "Calculated Risk Score Distribution",
        x = "Calculated Risk Score",
        y = "Number of Assets"
      ) +
      theme_minimal(base_size = 14)
  })
  
  
  # Risk vs Exposure
  output$risk_scatter <- renderPlot({
    
    ggplot(
      filtered_data(),
      aes(
        x = Open_Ports,
        y = Calculated_Risk,
        size = Dependency_Count,
        color = Risk_Level
      )
    ) +
      geom_point(alpha = 0.8) +
      labs(
        title = "Infrastructure Exposure vs Calculated Risk",
        x = "Open Ports",
        y = "Calculated Risk Score",
        size = "Dependencies"
      ) +
      theme_minimal(base_size = 14)
  })
  
  
  # Risk Components
  output$risk_components <- renderPlot({
    
    component_data <- data.frame(
      Component = c(
        "Public Exposure",
        "Open Ports",
        "IAM Risk",
        "Dependency Risk",
        "Asset Criticality"
      ),
      Score = c(
        mean(data$Exposure_Score),
        mean(data$Port_Score),
        mean(data$IAM_Score),
        mean(data$Dependency_Score),
        mean(data$Criticality_Score)
      )
    )
    
    ggplot(
      component_data,
      aes(x = Component, y = Score, fill = Component)
    ) +
      geom_col(width = 0.7) +
      labs(
        title = "Average Risk Score Components",
        x = "Risk Factor",
        y = "Average Contribution"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position = "none"
      )
  })
  
  
  # Recommendation Table
  output$recommendation_table <- renderDT({
    
    recommendations <- filtered_data() %>%
      filter(Misconfiguration_Type != "No Misconfiguration") %>%
      select(
        Asset_ID,
        Resource_Name,
        Resource_Type,
        Risk_Level,
        Calculated_Risk,
        Misconfiguration_Type,
        Recommendation
      ) %>%
      arrange(desc(Calculated_Risk))
    
    datatable(
      recommendations,
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "print"),
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  
  # ========================================================
  # SUMMARY REPORT (NEW)
  # ========================================================
  
  # Reactive summary table grouped by Resource Type
  summary_data <- reactive({
    
    filtered_data() %>%
      group_by(Resource_Type) %>%
      summarise(
        Total_Assets = n(),
        Misconfigured_Assets = sum(Misconfiguration_Type != "No Misconfiguration"),
        Pct_Misconfigured = round(
          100 * sum(Misconfiguration_Type != "No Misconfiguration") / n(),
          1
        ),
        Avg_Risk_Score = round(mean(Calculated_Risk, na.rm = TRUE), 1),
        Critical_Count = sum(Risk_Level == "Critical"),
        High_Count = sum(Risk_Level == "High"),
        Medium_Count = sum(Risk_Level == "Medium"),
        Low_Count = sum(Risk_Level == "Low"),
        Public_Exposure_Count = sum(Public_Access == "Yes"),
        High_IAM_Risk_Count = sum(IAM_Risk == "High"),
        .groups = "drop"
      ) %>%
      arrange(desc(Avg_Risk_Score))
    
  })
  
  
  # KPI: Total Assets in current filter
  output$summary_total_assets <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Total Assets in View",
      icon = icon("cubes"),
      color = "blue"
    )
  })
  
  
  # KPI: Total Misconfigured Assets
  output$summary_total_misconfig <- renderValueBox({
    valueBox(
      value = sum(filtered_data()$Misconfiguration_Type != "No Misconfiguration"),
      subtitle = "Misconfigured Assets",
      icon = icon("bug"),
      color = "red"
    )
  })
  
  
  # KPI: Percentage Misconfigured
  output$summary_pct_misconfig <- renderValueBox({
    
    total <- nrow(filtered_data())
    
    pct <- if (total > 0) {
      round(
        100 * sum(filtered_data()$Misconfiguration_Type != "No Misconfiguration") / total,
        1
      )
    } else {
      0
    }
    
    valueBox(
      value = paste0(pct, "%"),
      subtitle = "Misconfiguration Rate",
      icon = icon("percent"),
      color = "orange"
    )
  })
  
  
  # KPI: Average Risk (Summary Tab)
  output$summary_avg_risk <- renderValueBox({
    valueBox(
      value = round(mean(filtered_data()$Calculated_Risk, na.rm = TRUE), 1),
      subtitle = "Average Risk Score",
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  
  # Summary Table by Resource Type
  output$summary_table <- renderDT({
    
    result <- summary_data()
    
    # Overall totals row
    totals_row <- data.frame(
      Resource_Type = "TOTAL / OVERALL",
      Total_Assets = sum(result$Total_Assets),
      Misconfigured_Assets = sum(result$Misconfigured_Assets),
      Pct_Misconfigured = round(
        100 * sum(result$Misconfigured_Assets) / sum(result$Total_Assets),
        1
      ),
      Avg_Risk_Score = round(
        mean(filtered_data()$Calculated_Risk, na.rm = TRUE),
        1
      ),
      Critical_Count = sum(result$Critical_Count),
      High_Count = sum(result$High_Count),
      Medium_Count = sum(result$Medium_Count),
      Low_Count = sum(result$Low_Count),
      Public_Exposure_Count = sum(result$Public_Exposure_Count),
      High_IAM_Risk_Count = sum(result$High_IAM_Risk_Count)
    )
    
    result <- rbind(result, totals_row)
    
    datatable(
      result,
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "print"),
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE,
      colnames = c(
        "Resource Type",
        "Total Assets",
        "Misconfigured Assets",
        "% Misconfigured",
        "Avg Risk Score",
        "Critical",
        "High",
        "Medium",
        "Low",
        "Public Exposure",
        "High IAM Risk"
      )
    ) %>%
      formatStyle(
        "Avg Risk Score",
        backgroundColor = styleInterval(
          c(40, 60, 80),
          c("#28a745", "#f39c12", "#dc3545", "#8b0000")
        ),
        color = "white",
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Resource Type",
        target = "row",
        fontWeight = styleEqual("TOTAL / OVERALL", "bold"),
        backgroundColor = styleEqual("TOTAL / OVERALL", "#dfe6ee")
      )
  })
  
}


# ==========================================================
# 11. RUN APPLICATION
# ==========================================================

shinyApp(ui = ui, server = server)