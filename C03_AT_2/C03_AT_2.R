# ============================================================
# CO3 - ASSESSMENT TASK 2
# Security Verification Experiment: DAST + SCA
# Dynamic Application Security Testing and Software Composition
# Analysis using Synthetic Data
# ============================================================
#
# Run:
#   install.packages(c("shiny","ggplot2","DT","DiagrammeR","dplyr"))
#   shiny::runApp("C03_AT_2.R")
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

set.seed(2206)

# ---------------------- SYNTHETIC DATA ------------------------
make_dast_data <- function(n=140) {
  endpoints <- c("/login","/api/users","/api/payment","/upload",
                 "/admin","/search","/profile","/reports")
  findings <- c("SQL Injection","XSS","Broken Authentication",
                "Security Misconfiguration","Path Traversal",
                "Insecure CORS","Sensitive Data Exposure")
  sev <- c("Critical","High","Medium","Low")
  
  d <- data.frame(
    Test_ID=sprintf("DAST-%03d",1:n),
    Endpoint=sample(endpoints,n,TRUE),
    Finding=sample(findings,n,TRUE),
    Severity=sample(sev,n,TRUE,prob=c(.10,.25,.43,.22)),
    HTTP_Status=sample(c(200,201,302,400,401,403,404,500),n,TRUE),
    Response_Time_ms=round(rlnorm(n,log(280),.35)),
    Payloads_Tested=sample(40:500,n,TRUE),
    stringsAsFactors=FALSE
  )
  d$DAST_Detected <- rbinom(n,1,runif(n,.75,.98))
  d
}

make_sca_data <- function(n=120) {
  packages <- c("log4j","lodash","axios","openssl","spring-core",
                "requests","django","express","jquery","jackson")
  licenses <- c("MIT","Apache-2.0","BSD","GPL-3","LGPL-2.1")
  sev <- c("Critical","High","Medium","Low")
  
  d <- data.frame(
    SCA_ID=sprintf("SCA-%03d",1:n),
    Package=sample(packages,n,TRUE),
    Version=paste0(sample(1:9,n,TRUE),".",sample(0:12,n,TRUE)),
    License=sample(licenses,n,TRUE),
    Severity=sample(sev,n,TRUE,prob=c(.08,.27,.45,.20)),
    CVE_Severity_Score=round(pmin(10,pmax(1,rnorm(n,6.1,2.1))),1),
    stringsAsFactors=FALSE
  )
  d$SCA_Detected <- rbinom(n,1,runif(n,.80,.99))
  d
}

dast <- make_dast_data()
sca <- make_sca_data()

# --------------------------- CSS ------------------------------
app_css <- "
body { background:#f7f8fc; font-family:Cambria, Georgia, serif; }
h2,h3,h4 { font-family:Cambria, Georgia, serif; font-weight:700; }
.navbar { background:#263238 !important; }
.navbar-brand,.navbar-nav > li > a { color:white !important; font-weight:700; }
.info-box { background:white; border-radius:14px; padding:18px; margin:14px 0;
            box-shadow:0 2px 10px rgba(0,0,0,.08); border:1px solid #dce2ea; }
table.dataTable thead th { background:#37474F; color:white; font-weight:700; }
"

server <- function(input, output, session) {
  fd <- reactive({
    x <- dast
    if (input$dsev != "All") x <- x[x$Severity==input$dsev,]
    x
  })
  
  fs <- reactive({
    x <- sca
    if (input$ssev != "All") x <- x[x$Severity==input$ssev,]
    x
  })
  
  output$dast_total <- renderText(nrow(fd()))
  output$dast_detect <- renderText(sum(fd()$DAST_Detected))
  output$sca_total <- renderText(nrow(fs()))
  output$sca_detect <- renderText(sum(fs()$SCA_Detected))
  
  output$arch <- renderGrViz({
    grViz("
      digraph verification {
        graph [rankdir=TB, bgcolor='transparent', nodesep=.35, ranksep=.45]
        node [shape=box, style='rounded,filled', fontname='Cambria Bold',
              fontsize=18, margin='.20,.12', color='#263238', penwidth=2]
        edge [color='#78909C', penwidth=2, arrowsize=.7]
        A [label='RUNNING APPLICATION', fillcolor='#DDEBF7']
        B [label='TRAFFIC / API REQUESTS', fillcolor='#E2F0D9']
        C [label='DAST ENGINE', fillcolor='#FFF2CC']
        D [label='DEPENDENCY INVENTORY', fillcolor='#FCE4D6']
        E [label='SCA ENGINE', fillcolor='#E4DFEC']
        F [label='SECURITY VERIFICATION', fillcolor='#D9EAD3']
        G [label='RISK REPORT + REMEDIATION', fillcolor='#DDEBF7']
        A -> B -> C -> F -> G
        A -> D -> E -> F
      }
    ")
  })
  
  output$comparison <- renderPlot({
    d <- data.frame(
      Tool=c("DAST","SCA"),
      Findings=c(nrow(fd()),nrow(fs())),
      Detected=c(sum(fd()$DAST_Detected),sum(fs()$SCA_Detected))
    )
    d2 <- rbind(
      data.frame(Tool=d$Tool,Metric="Total Findings",Value=d$Findings),
      data.frame(Tool=d$Tool,Metric="Detected",Value=d$Detected)
    )
    ggplot(d2,aes(Tool,Value,fill=Metric)) +
      geom_col(position="dodge",width=.65) +
      geom_text(aes(label=Value),position=position_dodge(.65),vjust=-.35,fontface="bold") +
      scale_fill_manual(values=c("Total Findings"="#5B9BD5","Detected"="#70AD47")) +
      labs(title="DAST vs SCA Security Verification",x=NULL,y="Count") +
      theme_minimal(base_family="Cambria",base_size=14) +
      theme(plot.title=element_text(face="bold"))
  })
  
  output$dast_plot <- renderPlot({
    ggplot(fd() |> count(Severity),aes(reorder(Severity,-n),n,fill=Severity)) +
      geom_col(width=.68) +
      geom_text(aes(label=n),vjust=-.35,fontface="bold") +
      scale_fill_manual(values=c("Critical"="#C00000","High"="#ED7D31",
                                 "Medium"="#FFC000","Low"="#70AD47")) +
      labs(title="DAST Severity Distribution",x=NULL,y="Findings") +
      theme_minimal(base_family="Cambria",base_size=14) +
      theme(legend.position="none",plot.title=element_text(face="bold"))
  })
  
  output$sca_plot <- renderPlot({
    ggplot(fs(),aes(x=CVE_Severity_Score,fill=Severity)) +
      geom_histogram(binwidth=1,alpha=.9,color="white") +
      scale_fill_manual(values=c("Critical"="#C00000","High"="#ED7D31",
                                 "Medium"="#FFC000","Low"="#70AD47")) +
      labs(title="SCA CVE Severity Score Distribution",
           x="Severity Score",y="Packages") +
      theme_minimal(base_family="Cambria",base_size=14) +
      theme(plot.title=element_text(face="bold"))
  })
  
  output$dast_table <- renderDT({
    datatable(fd() |> mutate(DAST_Detected=ifelse(DAST_Detected==1,"YES","NO")),
              rownames=FALSE,options=list(pageLength=10,scrollX=TRUE))
  })
  
  output$sca_table <- renderDT({
    datatable(fs() |> mutate(SCA_Detected=ifelse(SCA_Detected==1,"YES","NO")),
              rownames=FALSE,options=list(pageLength=10,scrollX=TRUE))
  })
  
  output$summary <- renderDT({
    bind_rows(
      fd() |> summarise(Tool="DAST",Total=n(),Detected=sum(DAST_Detected),
                        Detection_Rate=round(mean(DAST_Detected)*100,1)),
      fs() |> summarise(Tool="SCA",Total=n(),Detected=sum(SCA_Detected),
                        Detection_Rate=round(mean(SCA_Detected)*100,1))
    ) |>
      mutate(Detection_Rate=paste0(Detection_Rate,"%")) |>
      datatable(rownames=FALSE,options=list(dom="t"))
  })
}

ui <- navbarPage(
  title="CO3 • AT-2 • DAST + SCA",
  header=tags$head(tags$style(HTML(app_css))),
  
  tabPanel("Dashboard",
           fluidRow(
             column(3,div(class="info-box",h4("DAST Findings"),h2(textOutput("dast_total")))),
             column(3,div(class="info-box",h4("DAST Detected"),h2(textOutput("dast_detect")))),
             column(3,div(class="info-box",h4("SCA Findings"),h2(textOutput("sca_total")))),
             column(3,div(class="info-box",h4("SCA Detected"),h2(textOutput("sca_detect"))))
           ),
           fluidRow(
             column(3,wellPanel(
               h4("DAST Filter"),
               selectInput("dsev","Severity",c("All","Critical","High","Medium","Low"))
             )),
             column(3,wellPanel(
               h4("SCA Filter"),
               selectInput("ssev","Severity",c("All","Critical","High","Medium","Low"))
             )),
             column(6,div(class="info-box",h3("Security Verification Architecture"),
                          grVizOutput("arch",height="400px")))
           ),
           fluidRow(
             column(6,div(class="info-box",plotOutput("comparison",height="350px"))),
             column(6,div(class="info-box",plotOutput("dast_plot",height="350px")))
           ),
           fluidRow(
             column(6,div(class="info-box",plotOutput("sca_plot",height="350px"))),
             column(6,div(class="info-box",h3("Verification Summary"),DTOutput("summary")))
           )
  ),
  
  tabPanel("DAST Simulation",
           div(class="info-box",
               h3("Synthetic Dynamic Application Security Testing Dataset"),
               p("DAST findings are simulated from application endpoints, HTTP behaviour and payload testing."),
               DTOutput("dast_table"))
  ),
  
  tabPanel("SCA Simulation",
           div(class="info-box",
               h3("Synthetic Software Composition Analysis Dataset"),
               p("SCA findings represent dependency, version, license and CVE-related observations."),
               DTOutput("sca_table"))
  ),
  
  tabPanel("About",
           div(class="info-box",
               h3("CO3 - Assessment No.2"),
               p(strong("Assessment Type: "), "Security Verification Experiment"),
               p(strong("Activity: "), "Students simulate DAST and SCA to evaluate application security."),
               h4("Expected Outcome"),
               tags$ul(
                 tags$li("Simulate runtime security testing."),
                 tags$li("Simulate third-party dependency analysis."),
                 tags$li("Compare DAST and SCA findings."),
                 tags$li("Evaluate detection performance and security verification.")
               ))
  )
)

shinyApp(ui, server)
