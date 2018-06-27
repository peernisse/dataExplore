#-----------------On Load Component---------------------------
#Libraries
library(shiny)
library(tidyverse)


#Source functions
source('helpers.R')

#-----------------UI Component--------------------------------
ui <- fluidPage(
        
   # Application title
   titlePanel("Data Explore"),
   
   # Data input and plot parameter tools on left
   sidebarLayout(
        sidebarPanel(
              h3("File Input"),
              p("Your .csv file should have columns: 'Location','Date','Parameter','Value','Units'"),
              fileInput("file", label = NULL,accept='.csv'),
              
              fluidRow(column(4, verbatimTextOutput("value"))),
              
              h3("Explore Tools"),
              fluidRow(
                      column(6,
                             h5(strong("Choose Locations")),
                             uiOutput('choose_locs')  
                      ),
                      column(6,
                             h5(strong("Choose Parameters")),
                             uiOutput('choose_params') 
                      )
              ),
              
              # h5(strong("Choose Locations")),
              # uiOutput('choose_locs'),
              # h5(strong("Choose Parameters")),
              # uiOutput('choose_params'),
              
              h5(strong("Choose Date Range")),
              fluidRow(
                      column(10,uiOutput('choose_dates')),
                      column(2,uiOutput('reset_dates'))
              )
              
              
              
              
        ),
      
      # plot preview and data export tools
      mainPanel(
         
              tabsetPanel(type = "tabs",
                          tabPanel("Table", tags$div(dataTableOutput('tblData'),style="height=600px")),
                          tabPanel("Explore Plots", 
                                   fluidRow(
                                           column(6,plotOutput("timeSeriesPlot")),
                                           column(6,plotOutput("boxPlot"))
                                           ),
                                   fluidRow(
                                           column(6,plotOutput("qPlot")),
                                           column(6,plotOutput("hPlot"))
                                   )
                                ),
                          tabPanel("Multiple Plots","Coming Soon"),
                          tabPanel("Stats", dataTableOutput('statsData'))
              )
                    
      )
   )
)

#-----------------Server Component----------------------------
server <- function(input, output, session) {
   
        pData <- reactive({
                
                inFile <- input$file
                
                if (is.null(inFile))
                        return(NULL)
                #Read in table
                tbl <- read.csv(inFile$datapath, header=TRUE, stringsAsFactors = FALSE)
                
                tbl$Date<-as.POSIXct(strptime(tbl$Date,format="%d-%b-%y"))
                #Fix unit cases
                tbl$Units<-fixUnits(tbl)
                
                return(tbl)
        })
        
        baseDates <- reactive({
                inFile <- input$file
                
                if (is.null(inFile))
                        return(NULL)
                
                dts <- read.csv(inFile$datapath, header=TRUE, stringsAsFactors = FALSE)
                dts$Date<-as.POSIXct(strptime(dts$Date,format="%d-%b-%y"))
                bDates<-c(min(dts$Date),max(dts$Date))
                
                return(bDates)#This could possibly combined into pData() as a separate output and referenced pData()$bDates
                
        })
        
        #Date range reset button action
        observeEvent(input$clrDates,{
                updateDateRangeInput(session,inputId = 'dtRng', start = baseDates()[1], end = baseDates()[2])
        })
        
        #Create location picker
        output$choose_locs<-renderUI({
                if(is.null(pData()))
                        return()
                locs<-unique(pData()$Location)
                
                checkboxGroupInput('locids',NULL,
                                   choices = locs)
        })
        
        #Create parameter picker
        output$choose_params<-renderUI({
                if(is.null(pData()))
                        return()
                params<-unique(pData()$Parameter)
                
                checkboxGroupInput('params',NULL,
                                   choices = params)
        })
        
        #Create date range input
        output$choose_dates<-renderUI({
                if(is.null(pData()))
                        return()
                # minDate<-min(pData()$Date)
                # maxDate<-max(pData()$Date)
                
                dateRangeInput('dtRng',NULL,
                               start = baseDates()[1],
                               end = baseDates()[2],
                               format = 'dd-M-yyyy'
                                
                               )
                
        })
        
        #Create reset date range button
        output$reset_dates <- renderUI({
                if(is.null(pData()))
                        return()
                actionButton('clrDates',NULL,
                             icon("refresh"), 
                             style="color: #fff; background-color: #337ab7; 
                             border-color: #2e6da4; font-size:90%; width:35px; 
                             height:30px; margin:1px")
        })
        
        output$tblData <- renderDataTable({
                if(is.null(input$locids))
                        return()
                
                fData<-pData() %>% filter(Location %in% input$locids,
                                          Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                          Parameter %in% input$params) %>% 
                        arrange(Location,Date)
                fData$Date<-as.character(fData$Date)
                
                return(fData)
        })
        
        output$timeSeriesPlot <- renderPlot({
                if(is.null(input$locids))
                        return()
                tsData<-as.data.frame(filter(pData(),Location %in% input$locids,
                                             Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                             Parameter %in% input$params))
                tsp<-tsPlot(tsData,tsData$Date,tsData$Value)
                return(tsp)
        })
        
        output$boxPlot <- renderPlot({
                if(is.null(input$locids))
                        return()
                bxData<-as.data.frame(filter(pData(),Location %in% input$locids,
                                             Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                             Parameter %in% input$params))
                bxp<-bxPlot(bxData,bxData$Location,bxData$Value)
                return(bxp)
        })
        
        output$qPlot <- renderPlot({
                if(is.null(input$locids))
                        return()
                qData<-as.data.frame(filter(pData(),Location %in% input$locids,
                                             Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                            Parameter %in% input$params))
                qp<-qPlot(qData)
                return(qp)
        })
        
        output$hPlot <- renderPlot({
                if(is.null(input$locids))
                        return()
                hData<-as.data.frame(filter(pData(),Location %in% input$locids,
                                            Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                            Parameter %in% input$params))
                hp<-hPlot(hData)
                return(hp)
        })
        
        output$statsData <- renderDataTable({
                if(is.null(input$locids))
                        return()
                pData() %>% filter(Location %in% input$locids,
                                   Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                   Parameter %in% input$params) %>% 
                        group_by(Parameter,Location) %>% 
                        summarise(`Number of Observations`=length(Value),
                                  `Minimum Date`=min(Date),
                                  `Maximum Date`=max(Date),
                                  Minimum=min(Value),
                                  Maximum=max(Value),
                                  `First Quartile`=quantile(Value,0.25),
                                  `Third Quartile`=quantile(Value,0.75),
                                  Average=mean(Value),
                                  `Standard Deviaton`=sd(Value),
                                  Variance=var(Value))
        })
        
}

#-----------------App Component--------------------------------
shinyApp(ui = ui, server = server)

