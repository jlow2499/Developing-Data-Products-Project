require(shiny)
require(shinydashboard)
require(googleVis)
require(DT)
require(dplyr)
require(xts)
require(dygraphs)

setwd("~/")

complaints <- read.csv("~/complaints.csv",stringsAsFactors=FALSE)
POP <- read.csv("C:/Users/193344/Desktop/State Charts/POP.csv",stringsAsFactors=FALSE)

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

cfpb.ts.sp <- complaints %>%
  group_by(Date.received,Sub.product) %>%
  summarize(Complaints = n()) %>%
  ungroup() %>%
  arrange(desc(Complaints))

cfpb.st.sp <- complaints %>%
  group_by(State,Sub.product) %>%
  summarize(Complaints = n()) %>%
  ungroup() %>%
  arrange(desc(Complaints))

bad <- c("","AA","AE","DC","AP","AS","FM","GU","MH","MP","PR","PW","VI")

cfpb.st <- cfpb.st[!cfpb.st$State %in% bad,]
cfpb.st.sp <- cfpb.st.sp[!cfpb.st.sp$State %in% bad,]

rm(complaints)

cfpb.st <- left_join(cfpb.st,POP,by="State") %>%
  mutate(ComplaintsToPopulation=round(Complaints/Population,7)*10000)

cfpb.st.sp <- left_join(cfpb.st.sp,POP,by="State") %>%
  mutate(ComplaintsToPopulation=round(Complaints/Population,7)*10000)

cfpb.st <-  plyr::rename(cfpb.st,c("ComplaintsToPopulation"="Complaints To Population * 10000"))
cfpb.st.sp <-  plyr::rename(cfpb.st.sp,c("ComplaintsToPopulation"="Complaints To Population * 10000"))

cfpb.st.sp <- rename(cfpb.st.sp,Product=Sub.product)
cfpb.ts.sp <- rename(cfpb.ts.sp,Product=Sub.product)

cfpb.st.sp <- cfpb.st.sp[cfpb.st.sp$Product != "",]
cfpb.ts.sp <- cfpb.ts.sp[cfpb.ts.sp$Product != "",]

cfpb.st$State <- factor(cfpb.st$State)
cfpb.st$Product <- factor(cfpb.st$Product)
cfpb.ts$Product <- factor(cfpb.ts$Product)

cfpb.st.sp$State <- factor(cfpb.st.sp$State)
cfpb.st.sp$Product <- factor(cfpb.st.sp$Product)
cfpb.ts.sp$Product <- factor(cfpb.ts.sp$Product)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("State Data Table", tabName = "state", icon = icon("dashboard")),
    menuItem("State Complaint Map",tabName = "MAP",icon = icon("bar-chart-o")),
    menuItem("Daily Data Table",tabName = "Day", icon = icon("dashboard")),
    menuItem("Daily Time Series Plot",tabName = "TZ",icon = icon("bar-chart-o")),
    selectInput("data",
                "Data View",
                choices=c("Product","Sub Product"),
                multiple=FALSE),
    uiOutput("input1"),
    fluidRow(column(width=1),actionButton("generate","Generate State Plot"))
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
                         uiOutput("input2")))),
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
  
  ProductView.st <- reactive({
    switch(input$data,
           "Product"=cfpb.st,
           "Sub Product"=cfpb.st.sp)
  })
  
  ProductView.ts <- reactive({
    switch(input$data,
           "Product"=cfpb.ts,
           "Sub Product"=cfpb.ts.sp)
  })
  
  output$input1 <- renderUI({
    if(is.null(input$data))
      return(NULL)
    Var <- ProductView.st()
    selectInput("product",
              "Select Product for State Map",
              choices=levels(Var$Product),
              multiple=FALSE)
  })
  
  output$input2 <- renderUI({
    if(is.null(input$data))
      return(NULL)
    Var <- ProductView.ts()
    selectInput("product2",
                "Select Product",
                choices=levels(Var$Product),
                multiple=FALSE)
  })
   
  output$day <- DT::renderDataTable({
    datatable(ProductView.ts(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
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
    datatable(ProductView.st(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
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
  
  plot1 <- eventReactive(input$generate,{
    state <- ProductView.st()
    state <- subset(state,Product == input$product)
    state
  })
  
 
  
  output$StatePlot <- renderGvis({
   gvisGeoChart(plot1(),"State","Complaints To Population * 10000",options=list(region="US", 
                                                                                 displayMode="regions", 
                                                                                 resolution="provinces",
                                                                                height=650,width=1100))

  })
  
  
  
  dygraph1 <- reactive({
    if(is.null(input$data))
      return(NULL)
    t <- ProductView.ts()
    t$Date.received <- as.Date(t$Date.received,format="%Y-%m-%d")
    t <- t[t$Product == input$product2,]
    t <- t[,-2]
    t <- as.xts(t,order.by=t$Date.received)
    t
  })
  
  output$DYEGRAPH1 <- renderDygraph({
   dygraph(dygraph1(),main="Complaints since 2012") %>%
      dyAxis("y",label = "Number of Complaints") %>%
      dyRangeSelector()
  })
  
}
shinyApp(ui, server)
