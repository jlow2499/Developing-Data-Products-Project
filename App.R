library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggvis)

setwd("C:/Users/193344/Desktop/DataProductsClass")

complaints <- read.csv("~/complaints.csv")

library(dplyr)

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
    menuItem("Daily Data Table",tabName = "Day", icon = icon("dashboard")),
    menuItem("State Plot",tabName = "MAP",icon = icon("bar-chart-o")),
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
            htmlOutput("StatePlot"))
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
  
}
shinyApp(ui, server)
