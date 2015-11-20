#load required packages
require(shiny)
require(shinydashboard)
require(googleVis)
require(DT)
require(dplyr)
require(xts)
require(dygraphs)

setwd("~/")

#read in files
complaints <- read.csv("~/complaints.csv",stringsAsFactors=FALSE)
POP <- read.csv("~/POP.csv",stringsAsFactors=FALSE)

#clean data
complaints$Date.received <- as.Date(complaints$Date.received,"%m/%d/%Y")

#create data tables using dplyr to group and summarize the variables of importance
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

#create a vector of states to remove from the data (for the state map)
bad <- c("","AA","AE","DC","AP","AS","FM","GU","MH","MP","PR","PW","VI")

cfpb.st <- cfpb.st[!cfpb.st$State %in% bad,]
cfpb.st.sp <- cfpb.st.sp[!cfpb.st.sp$State %in% bad,]

rm(complaints)

#join the created data frames to the state population data frame by State
cfpb.st <- left_join(cfpb.st,POP,by="State") %>%
  mutate(ComplaintsToPopulation=(Complaints/Population)*10000)

cfpb.st.sp <- left_join(cfpb.st.sp,POP,by="State") %>%
  mutate(ComplaintsToPopulation=(Complaints/Population)*10000)

#use plyr's rename to clean up the variable names for presentation. 
#Dplyr's rename will not allow the use of spaces and some characters.
cfpb.st <-  plyr::rename(cfpb.st,c("ComplaintsToPopulation"="Complaints To Population Times 10000"))
cfpb.st.sp <-  plyr::rename(cfpb.st.sp,c("ComplaintsToPopulation"="Complaints To Population Times 10000"))

cfpb.st.sp <- rename(cfpb.st.sp,Product=Sub.product)
cfpb.ts.sp <- rename(cfpb.ts.sp,Product=Sub.product)

cfpb.st.sp <- cfpb.st.sp[cfpb.st.sp$Product != "",]
cfpb.ts.sp <- cfpb.ts.sp[cfpb.ts.sp$Product != "",]

#set variables to factors so they can be filtered properly in the datatables.
cfpb.st$State <- factor(cfpb.st$State)
cfpb.st$Product <- factor(cfpb.st$Product)
cfpb.ts$Product <- factor(cfpb.ts$Product)

cfpb.st.sp$State <- factor(cfpb.st.sp$State)
cfpb.st.sp$Product <- factor(cfpb.st.sp$Product)
cfpb.ts.sp$Product <- factor(cfpb.ts.sp$Product)

#create a sidebar object in the UI interface
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("State Data Table", 
             tabName = "state", 
             icon = icon("table")),
    menuItem("State Complaint Map",
             tabName = "MAP",
             icon = icon("globe")),
    menuItem("Daily Data Table",
             tabName = "Day", 
             icon = icon("table")),
    menuItem("Daily Time Series Plot",
             tabName = "TZ",
             icon = icon("area-chart")),
    selectInput("data",
                "Data View",
                choices=c("Product","Sub Product"),
                multiple=FALSE)
  )
)
#create the UI body
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
            fluidRow(column(width=4),
            column(width=4,uiOutput("input1"))),
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

#set the ui title and add the sidebar and body objects
ui <- dashboardPage(
  dashboardHeader(title = "CFPB Complaints"),
  sidebar,
  body
)

#create the server side of the dashboard
server <- function(input, output) {
  
  #create a switch to chose between which product to view in the dashboard
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
  
  #create dynamic select input buttons based upon the user's product selection.
  output$input1 <- renderUI({
    if(is.null(input$data))
      return(NULL)
    Var <- ProductView.st()
    selectInput("product",
              "Select Product",
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
   
   #create data tables. Use extensions for copy, save, and print.
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
  #create a reactive subset for the state plot
  plot1 <- reactive({
    state <- ProductView.st()
    state <- subset(state,Product == input$product)
    state
  })
  
  #create the state plot and delay the rendering by 1 second in order for the switch to work. 
  output$StatePlot <- renderGvis({
    Sys.sleep(1)
   gvisGeoChart(plot1(),"State","Complaints To Population Times 10000",options=list(region="US", 
                                                                                 displayMode="regions", 
                                                                                 resolution="provinces",
                                                                                height=550,width=1000))

  })
  
  
  #create a reactive subset for the dygraph
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
  #create the dyegraph
  output$DYEGRAPH1 <- renderDygraph({
   dygraph(dygraph1(),main="Complaints since 2012") %>%
      dyAxis("y",label = "Number of Complaints") %>%
      dyRangeSelector()
  })
  
}
#create the Shiny app object
shinyApp(ui, server)
