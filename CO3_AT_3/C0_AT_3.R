# ============================================================
# CO3 - ASSESSMENT TASK 3
# Vulnerability Analysis and Performance Evaluation
# Integrated SAST + DAST + SCA Security Simulation
# ============================================================
#
# Run:
#   install.packages(c("shiny","ggplot2","DT","DiagrammeR","dplyr"))
#   shiny::runApp("C03_AT_3.R")
# ============================================================

required <- c("shiny","ggplot2","DT","DiagrammeR","dplyr")
missing <- required[!sapply(required, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  stop(paste0("Install required packages first: ", paste(missing, collapse = ", ")))
}

library(shiny)
library(ggplot2)
library(DT)
library(DiagrammeR)
library(dplyr)

set.seed(3306)

# ---------------- INTEGRATED SYNTHETIC DATA -------------------
make_integrated_data <- function(n=240) {
  tools <- c("SAST","DAST","SCA")
  sev <- c("Critical","High","Medium","Low")
  vuln <- c("Injection","Authentication","XSS","Secrets",
            "Misconfiguration","Dependency CVE","Access Control",
            "Input Validation","Cryptography")
  
  data.frame(
    Finding_ID=sprintf("VULN-%03d",1:n),
    Tool=sample(tools,n,TRUE,prob=c(.36,.34,.30)),
    Vulnerability_Type=sample(vuln,n,TRUE),
    Severity=sample(sev,n,TRUE,prob=c(.12,.27,.41,.20)),
    CVSS=round(pmin(10,pmax(1,rnorm(n,6.3,2.0))),1),
    Scan_Time_sec=round(runif(n,2,18),2),
    Remediation_Time_days=sample(1:30,n,TRUE),
    stringsAsFactors=FALSE
  ) |>
    mutate(
      Detection_Probability=case_when(
        Tool=="SAST" ~ runif(n(),.78,.98),
        Tool=="DAST" ~ runif(n(),.72,.96),
        TRUE ~ runif(n(),.80,.99)
      ),
      Detected=rbinom(n(),1,Detection_Probability),
      Security_Verified=rbinom(n(),1,
                               pmin(.99,.78 + ifelse(Detected==1,.15,0))),
      Risk_Score=round(CVSS *
                         case_when(Severity=="Critical"~1.40,
                                   Severity=="High"~1.20,
                                   Severity=="Medium"~1.10,
                                   TRUE~1.00),2)
    )
}

vulns <- make_integrated_data()

# -------------------- PERFORMANCE DATA -------------------------
performance <- vulns |>
  group_by(Tool) |>
  summarise(
    Findings=n(),
    Detected=sum(Detected),
    Detection_Rate=round(mean(Detected)*100,1),
    Verified=sum(Security_Verified),
    Verification_Rate=round(mean(Security_Verified)*100,1),
    Avg_CVSS=round(mean(CVSS),2),
    Avg_Risk=round(mean(Risk_Score),2),
    Avg_Scan_Time=round(mean(Scan_Time_sec),2),
    .groups="drop"
  )

app_css <- "
body { background:#f5f7fb; font-family:Cambria, Georgia, serif; }
h2,h3,h4 { font-family:Cambria, Georgia, serif; font-weight:700; }
.navbar { background:#17365D !important; }
.navbar-brand,.navbar-nav > li > a { color:white !important; font-weight:700; }
.info-box { background:white; border-radius:14px; padding:18px; margin:14px 0;
            box-shadow:0 2px 10px rgba(0,0,0,.08); border:1px solid #d8e0ea; }
table.dataTable thead th { background:#17365D; color:white; font-weight:700; }
"

server <- function(input, output, session) {
  filtered <- reactive({
    d <- vulns
    if (input$tool != "All") d <- d[d$Tool==input$tool,]
    if (input$sev != "All") d <- d[d$Severity==input$sev,]
    d
  })
  
  output$total <- renderText(nrow(filtered()))
  output$detected <- renderText(sum(filtered()$Detected))
  output$verified <- renderText(sum(filtered()$Security_Verified))
  output$rate <- renderText(paste0(round(mean(filtered()$Detected)*100,1),"%"))
  
  output$arch <- renderGrViz({
    grViz("
      digraph integrated {
        graph [rankdir=LR, bgcolor='transparent', nodesep=.40, ranksep=.55]
        node [shape=box, style='rounded,filled', fontname='Cambria Bold',
              fontsize=18, margin='.20,.12', color='#17365D', penwidth=2]
        edge [color='#6B7280', penwidth=2, arrowsize=.7]
        A [label='PLAN + CODE', fillcolor='#DDEBF7']
        B [label='SAST', fillcolor='#E2F0D9']
        C [label='DAST', fillcolor='#FFF2CC']
        D [label='SCA', fillcolor='#FCE4D6']
        E [label='VULNERABILITY\\nCORRELATION', fillcolor='#E4DFEC']
        F [label='SEVERITY + RISK\\nANALYSIS', fillcolor='#D9EAD3']
        G [label='SECURITY\\nVERIFICATION', fillcolor='#DDEBF7']
        H [label='PERFORMANCE\\nEVALUATION', fillcolor='#FFF2CC']
        A -> B -> E
        A -> C -> E
        A -> D -> E
        E -> F -> G -> H
      }
    ")
  })
  
  output$tool_plot <- renderPlot({
    d <- filtered() |> count(Tool)
    ggplot(d,aes(reorder(Tool,-n),n,fill=Tool)) +
      geom_col(width=.65) +
      geom_text(aes(label=n),vjust=-.35,fontface="bold",size=5) +
      scale_fill_manual(values=c("SAST"="#5B9BD5","DAST"="#ED7D31","SCA"="#70AD47")) +
      labs(title="Findings by Security Testing Technique",x=NULL,y="Findings") +
      theme_minimal(base_family="Cambria",base_size=14) +
      theme(legend.position="none",plot.title=element_text(face="bold"))
  })
  
  output$severity_plot <- renderPlot({
    d <- filtered() |> count(Severity)
    ggplot(d,aes(reorder(Severity,-n),n,fill=Severity)) +
      geom_col(width=.65) +
      geom_text(aes(label=n),vjust=-.35,fontface="bold",size=5) +
      scale_fill_manual(values=c("Critical"="#C00000","High"="#ED7D31",
                                 "Medium"="#FFC000","Low"="#70AD47")) +
      labs(title="Overall Vulnerability Severity",x=NULL,y="Findings") +
      theme_minimal(base_family="Cambria",base_size=14) +
      theme(legend.position="none",plot.title=element_text(face="bold"))
  })
  
  output$performance_plot <- renderPlot({
    d <- performance |>
      select(Tool,Detection_Rate,Verification_Rate) |>
      tidyr::pivot_longer(-Tool,names_to="Metric",values_to="Value")
    ggplot(d,aes(Tool,Value,fill=Metric)) +
      geom_col(position="dodge",width=.65) +
      geom_text(aes(label=paste0(Value,"%")),
                position=position_dodge(.65),vjust=-.35,fontface="bold") +
      scale_fill_manual(values=c("Detection_Rate"="#4472C4",
                                 "Verification_Rate"="#70AD47"),
                        labels=c("Detection Rate","Verification Rate")) +
      labs(title="Security Testing Performance",x=NULL,y="Percentage") +
      ylim(0,110) +
      theme_minimal(base_family="Cambria",base_size=14) +
      theme(plot.title=element_text(face="bold"))
  })
  
  output$risk_plot <- renderPlot({
    ggplot(filtered(),aes(CVSS,Risk_Score,fill=Severity)) +
      geom_point(shape=21,size=3.4,alpha=.78) +
      scale_fill_manual(values=c("Critical"="#C00000","High"="#ED7D31",
                                 "Medium"="#FFC000","Low"="#70AD47")) +
      labs(title="Risk Scoring and Vulnerability Severity",
           x="CVSS",y="Calculated Risk Score") +
      theme_minimal(base_family="Cambria",base_size=14) +
      theme(plot.title=element_text(face="bold"))
  })
  
  output$table <- renderDT({
    datatable(filtered() |> mutate(
      Detected=ifelse(Detected==1,"YES","NO"),
      Security_Verified=ifelse(Security_Verified==1,"YES","NO")
    ),rownames=FALSE,options=list(pageLength=14,scrollX=TRUE))
  })
  
  output$performance_table <- renderDT({
    datatable(performance,rownames=FALSE,options=list(dom="t"))
  })
  
  output$severity_table <- renderDT({
    filtered() |>
      group_by(Severity) |>
      summarise(
        Findings=n(),
        Detected=sum(Detected),
        Detection_Rate=paste0(round(mean(Detected)*100,1),"%"),
        Verified=sum(Security_Verified),
        Avg_Risk=round(mean(Risk_Score),2),
        Avg_Remediation_Days=round(mean(Remediation_Time_days),1),
        .groups="drop"
      ) |>
      datatable(rownames=FALSE,options=list(dom="t"))
  })
}

ui <- navbarPage(
  title="CO3 • AT-3 • Vulnerability Analysis",
  header=tags$head(tags$style(HTML(app_css))),
  
  tabPanel("Dashboard",
           fluidRow(
             column(3,div(class="info-box",h4("Total Findings"),h2(textOutput("total")))),
             column(3,div(class="info-box",h4("Detected"),h2(textOutput("detected")))),
             column(3,div(class="info-box",h4("Security Verified"),h2(textOutput("verified")))),
             column(3,div(class="info-box",h4("Detection Rate"),h2(textOutput("rate"))))
           ),
           fluidRow(
             column(3,wellPanel(
               h4("Analysis Filters"),
               selectInput("tool","Testing Technique",c("All","SAST","DAST","SCA")),
               selectInput("sev","Severity",c("All","Critical","High","Medium","Low"))
             )),
             column(9,div(class="info-box",h3("Integrated DevSecOps Security Architecture"),
                          grVizOutput("arch",height="300px")))
           ),
           fluidRow(
             column(6,div(class="info-box",plotOutput("tool_plot",height="340px"))),
             column(6,div(class="info-box",plotOutput("severity_plot",height="340px")))
           ),
           fluidRow(
             column(6,div(class="info-box",plotOutput("performance_plot",height="340px"))),
             column(6,div(class="info-box",plotOutput("risk_plot",height="340px")))
           )
  ),
  
  tabPanel("Vulnerability Analysis",
           div(class="info-box",
               h3("Integrated Vulnerability Simulation Table"),
               p("The dataset combines SAST, DAST and SCA observations for comparative analysis."),
               DTOutput("table"))
  ),
  
  tabPanel("Performance",
           div(class="info-box",h3("Tool-wise Performance Evaluation"),DTOutput("performance_table")),
           div(class="info-box",h3("Severity-wise Verification Analysis"),DTOutput("severity_table"))
  ),
  
  tabPanel("Conclusion",
           div(class="info-box",
               h3("CO3 - Assessment No.3"),
               p(strong("Assessment Type: "), "Vulnerability Analysis and Performance Evaluation"),
               p(strong("Activity: "), "Vulnerability detection, severity analysis, security verification workflow
          simulation and evaluation of overall security performance."),
               h4("Simulation Conclusion"),
               tags$ul(
                 tags$li("SAST provides early source-code vulnerability detection."),
                 tags$li("DAST validates runtime behaviour and exposed application interfaces."),
                 tags$li("SCA identifies vulnerable third-party dependencies."),
                 tags$li("Combining the techniques gives broader security coverage."),
                 tags$li("Detection rate, verification rate, CVSS and risk score can be used to evaluate performance."),
                 tags$li("The Shiny interface makes the simulated results easy to inspect and compare.")
               ))
  )
)

shinyApp(ui, server)
