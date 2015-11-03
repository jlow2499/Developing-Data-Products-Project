require(shiny)
require(shinydashboard)
require(googleVis)
require(DT)
require(dplyr)
require(xts)
require(dygraphs)

setwd("~/")

complaints <- read.csv("~/complaints.csv")

complaints$Date.received <- as.Date(complaints$Date.received,"%m/%d/%Y")

cfpb.ts <- complaints %>%
  group_by(Date.received,Product) %>%
  summarize(Complaints = n()) %>%
  ungroup() %>%
  arrange(desc(Complaints))

cfpb.st <- complaints %>%
  group_by(State,Product) %>%
  summarize(Complaints = n()) %>%
  ungroup() %>%
  arrange(desc(Complaints))

bad <- c("","AA","AE","DC","AP","AS","FM","GU","MH","MP","PR","PW","VI")

cfpb.st <- cfpb.st[!cfpb.st$State %in% bad,]
cfpb.st$State <- factor(cfpb.st$State)

rm(complaints)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("State Data Table", tabName = "state", icon = icon("dashboard")),
    menuItem("State Complaint Map",tabName = "MAP",icon = icon("bar-chart-o")),
    menuItem("Daily Data Table",tabName = "Day", icon = icon("dashboard")),
    menuItem("Daily Time Series Plot",tabName = "TZ",icon = icon("bar-chart-o")),
    selectInput("product",
                "Select Product for State Map",
                choices=levels(cfpb.st$Product),
                multiple=FALSE)
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "state",
            h2("State Data Table"),
            DT::dataTableOutput("state")
    ),
    tabItem(tabName ="Day",
            h2("Daily Data table"),
            DT::dataTableOutput("day")),
    tabItem(tabName="MAP",
            h2("State Map"),
            htmlOutput("StatePlot")),
    tabItem(tabName="TZ",
            h2("Time Series Plot"),
            
            fluidRow(
              column(width=4),
              column(width=4,
                     box(width=NULL,
                         selectInput("product2",
                                     "Select Product",
                                     choices=levels(cfpb.ts$Product),
                                     multiple=FALSE)))),
            box(width=12,
                dygraphOutput("DYEGRAPH1")))
    )
  )


ui <- dashboardPage(
  dashboardHeader(title = "CFPB Complaints"),
  sidebar,
  body
)

server <- function(input, output) {
  
   
  output$day <- DT::renderDataTable({
    datatable(cfpb.ts,extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
              options = list(
                searching=TRUE,
                autoWidth=TRUE,
                paging=TRUE,
                
                "sDom" = 'T<"clear">lfrtip',
                "oTableTools" = list(
                  "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                  "aButtons" = list(
                    "copy",
                    "print",
                    list("sExtends" = "collection",
                         "sButtonText" = "Save",
                         "aButtons" = c("csv","xls"))))))
  })
   
  output$state <- DT::renderDataTable({
    datatable(cfpb.st,extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
              options = list(
                searching=TRUE,
                autoWidth=TRUE,
                paging=FALSE,
                "sDom" = 'T<"clear">lfrtip',
                "oTableTools" = list(
                  "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                  "aButtons" = list(
                    "copy",
                    "print",
                    list("sExtends" = "collection",
                         "sButtonText" = "Save",
                         "aButtons" = c("csv","xls"))))))
  })
  
  dygraph1 <- reactive({
    t <- cfpb.ts[cfpb.ts$Product == input$product2,]
    t <- t[,-2]
    t <- as.xts(t,order.by=t$Date.received)
    t
  })
  
  plot1 <- reactive({
    state <- subset(cfpb.st,Product == input$product)
    state
  })
  
  output$StatePlot <- renderGvis({
    gvisGeoChart(plot1(),"State","Complaints",options=list(region="US", 
                                                           displayMode="regions", 
                                                           resolution="provinces",
                                                           width=1200, height=800))
  })
  
  output$DYEGRAPH1 <- renderDygraph({
    dygraph(dygraph1(),main="Complaints since 2012") %>%
      dyAxis("y",label = "Number of Complaints") %>%
      dyRangeSelector()
  })
  
}
shinyApp(ui, server)
