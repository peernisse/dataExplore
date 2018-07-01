#-----------------On Load Component---------------------------
#Libraries
library(shiny)
library(tidyverse)
library(markdown)
options(scipen = 6)


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
              fluidRow(
                      column(12,
                             p("Your .csv file should have columns: 'Location','Matrix',Date','Parameter','Value','Units',
                               'DetectionFlag','Reporting_Limit','MDL'")
                             )
                      
              ),
              
              fileInput("file", label = NULL,accept='.csv'),
              
              fluidRow(column(4, verbatimTextOutput("value"))),
              
              h3("Explore Tools"),
              h5(strong('Choose Matrices')),
              uiOutput('choose_matrix'),
              fluidRow(
                      column(6,
                             h5(strong("Choose Locations")),
                             
                             wellPanel(id='locPanel',style = "overflow-y:scroll; max-height: 180px",
                                     uiOutput('choose_locs')
                                     
                             )
                               
                      ),
                      column(6,
                             h5(strong("Choose Parameters")),
                             wellPanel(id='paramPanel',style = "overflow-y:scroll; max-height: 180px",
                                       uiOutput('choose_params')
                                       
                             )
                              
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
                                        plotOutput("timeSeriesPlot"),
                                        plotOutput("boxPlot"),
                                        plotOutput("qPlot"),
                                        plotOutput("hPlot")
                          ),
                          tabPanel("Multiple Plots","Coming Soon"),
                          
                          tabPanel("Stats",
                                   
                                   fluidRow(
                                           
                                           column(6, h3('Select Stats to Show'),uiOutput('choose_stats')),
                                           column(6,h3('Output this Table to CSV'),
                                                  fluidRow(
                                                          column(6,textInput('expStatsFilename',label=NULL,width = '200px',placeholder = 'enter filename')),
                                                          column(1,h4('.csv'),style='padding-left: 0px;'),
                                                          column(2,downloadButton('expStats','Download'))
                                                          
                                                  )
                                           )
                                   ),
                                   
                                   hr(),
                                   
                                   dataTableOutput('statsData')
                                   
                                         
                          ),#tab panel stats
                          
                          tabPanel("About",
                                   includeMarkdown('www/readme.md'))
              )#tabset panel
                    
      )#main panel
   )#sidebar layout
)#fluid page

#-----------------Server Component----------------------------
server <- function(input, output, session) {
   
        #Data import-----------------------------------------
        pData <- reactive({
                
                inFile <- input$file
                
                if (is.null(inFile))
                        return(NULL)
                #Read in table
                tbl <- read.csv(inFile$datapath, header=TRUE, stringsAsFactors = FALSE)
                #Fix date format
                tbl$Date<-as.POSIXct(strptime(tbl$Date,format="%d-%b-%y"))
                #Fix unit cases
                tbl$Units<-fixUnits(tbl)
                #Make non detect substitution columns
                tbl$Result_ND<-as.numeric(ifelse(tbl$DetectionFlag=='ND',tbl$Reporting_Limit*0.5,tbl$Value))
                tbl$NonDetect<-as.numeric(ifelse(tbl$DetectionFlag=='ND',tbl$Reporting_Limit*0.5,''))
                
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
        
        #Buttons-------------------------------------------------
        #Date range refresh button action
        observeEvent(input$clrDates,{
                updateDateRangeInput(session,inputId = 'dtRng', start = baseDates()[1], end = baseDates()[2])
        })
        
        #Create refresh date range button
        output$reset_dates <- renderUI({
                if(is.null(pData()))
                        return()
                actionButton('clrDates',NULL,
                             icon("refresh"), 
                             style="color: #fff; background-color: #337ab7; 
                             border-color: #2e6da4; font-size:90%; width:35px; 
                             height:30px; margin:1px")
        })
        
        #Update locations and parameters checkboxes
        
        #Create locations and parameters checkboxes button
        
        #Pickers----------------------------------------------------
        #Create location picker
        output$choose_locs<-renderUI({
                if(is.null(pData()))
                        return()
                locs<-sort(unique(pData()$Location))
                
                checkboxGroupInput('locids',NULL,
                                   choices = locs)
        })
        
        #Create parameter picker
        output$choose_params<-renderUI({
                if(is.null(pData()))
                        return()
                params<-sort(unique(pData()$Parameter))
                
                checkboxGroupInput('params',NULL,
                                   choices = params)
        })
        
        #Create matrix picker
        output$choose_matrix<-renderUI({
                if(is.null(pData()))
                        return()
                matrices<-sort(unique(pData()$Matrix))
                
                checkboxGroupInput('mtrx',NULL,
                                   choices = matrices,
                                   selected = matrices,
                                   inline = TRUE)
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
        
        #Create stats table column picker
        output$choose_stats<-renderUI({
                if(is.null(pData()))
                        return()
                
                #statList<-names(statSumm())
                #statList<-statList[-c(1:3)]
                
                checkboxGroupInput('stats',NULL,
                                   choices = statList,
                                   selected = statList,
                                   inline = TRUE)
                
        })
        
        #Data table output
        #Raw data------------------------------
        
        output$tblData <- renderDataTable({
                if(is.null(input$locids))
                        return()
                
                fData<-pData() %>% filter(Location %in% input$locids,Matrix %in% input$mtrx,
                                          Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                          Parameter %in% input$params) %>% 
                        arrange(Location,Date)
                fData$Date<-as.character(fData$Date)
                
                return(fData)
        })
        
        #Stats summary data table---------------------------
        output$statsData <- renderDataTable({
                if(is.null(input$locids)|is.null(input$stats))
                        return()
                #sData <- statSumm() %>% select(input$stats)
                sData <- statSumm()
                return(sData)
        })
        
        statSumm <- reactive({
                
                pData() %>% filter(Location %in% input$locids,Matrix %in% input$mtrx,
                                   Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                   Parameter %in% input$params) %>% 
                        group_by(Matrix, Parameter,Location) %>% 
                        summarise(`Number of Observations`=length(Value),
                                  `Percent Non-Detect`=round(perND(DetectionFlag,'ND'),0),
                                  `Minimum Date`=min(Date),
                                  `Maximum Date`=max(Date),
                                  Minimum=as.character(ifelse(min(Value)==0,'ND',min(Value))),
                                  Maximum=max(Value),
                                  `First Quartile`=as.character(ifelse(quantile(Value,0.25)==0,'ND',quantile(Value,0.25))),
                                  `Third Quartile`=as.character(ifelse(quantile(Value,0.75)==0,'ND',quantile(Value,0.75))),
                                  Average=mean(Value),
                                  `Standard Deviation`=sd(Value),
                                  Variance=var(Value)) %>% 
                        select(1:3,input$stats)
                
        })
        
        
        
        #Export stats summary table download button------------------
        #Output handler
        output$expStats <- downloadHandler(
                                
                                filename = function() {
                                        paste(input$expStatsFilename, ".csv", sep = "")
                                },
                                
                                content = function(file) {
                                        write.csv(statSumm(), file, row.names = FALSE)
                                }
                           )
     
        
        #Plotting-----------------------------------------------
        #Explore plots--------------------------------
        output$timeSeriesPlot <- renderPlot({
                if(is.null(input$locids))
                        return()
                tsData<-as.data.frame(filter(pData(),Location %in% input$locids,
                                             Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                             Parameter %in% input$params))
                tsp<-tsPlot(tsData)
                return(tsp)
        })
        
        output$boxPlot <- renderPlot({
                if(is.null(input$locids))
                        return()
                bxData<-as.data.frame(filter(pData(),Location %in% input$locids,
                                             Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                             Parameter %in% input$params))
                bxp<-bxPlot(bxData)
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
        
        #Looped plots on a page
        
        
        
        
}

#-----------------App Component--------------------------------
shinyApp(ui = ui, server = server)

