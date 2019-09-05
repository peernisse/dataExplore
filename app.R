#-----------------On Load Component---------------------------
#Libraries
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(markdown)
library(data.table)
#library(RODBC)
options(scipen = 6)


#Source functions
source('helpers.R')

#TESTING DATA INPUT

#pData<-read.csv('C:/Shiny/data/cerroVerde_GW.csv',stringsAsFactors = FALSE)

#Demo data frame
#demoData<-read.csv('demoData.csv',stringsAsFactors = FALSE)

#-----------------UI Component--------------------------------
ui <- fluidPage(
        
   # Application title
   titlePanel("Data Explore"),
   
   # Data input and plot parameter tools on left
   sidebarLayout(
        sidebarPanel(
              h3("CSV File Input"),
              fluidRow(
                      column(12,
                             p("Your .csv file should have columns: 'SiteID','Site','Location','Matrix',Date','Parameter','Value','Units',
                               'DetectionFlag','ReportingLimit','MDL'")
                             )
                      
              ),
              hr(),
              
              fileInput("file", buttonLabel = 'Choose File',label=NULL,placeholder = 'Loading may take some time',accept='.csv'),
              
              h4('Or'),
              
              actionButton(inputId = 'demoLoad',label = 'Load Demo Data'),
              
              hr(),
              h3("Explore Tools"),
              
              fluidRow(
                    column(6,
                           h5(strong('Choose Matrices')),
                           actionLink('selectall_Matrix','Select All | Clear All'),
                           wellPanel(id='sitePanel',style = "overflow-y:scroll; max-height: 180px",
                                     uiOutput('choose_matrix')
                                )
                              
                           ),
                    
                    column(6,
                           h5(strong('Choose Sites')),
                           actionLink('selectall_Sites','Select All | Clear All'),
                           wellPanel(id='sitePanel',style = "overflow-y:scroll; max-height: 180px",
                                     uiOutput('choose_sites')
                                        )
                               
                    )
                      
              ),
              
              
              fluidRow(
                      column(6,
                             h5(strong("Choose Locations")),
                             actionLink("selectall_Locs","Select All | Clear All"),
                             
                             wellPanel(id='locPanel',style = "overflow-y:scroll; max-height: 180px",
                                     uiOutput('choose_locs')
                                     
                             )
                               
                      ),
                      column(6,
                             h5(strong("Choose Parameters")),
                             actionLink('selectall_Params','Select All | Clear All'),
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
                          
                          tabPanel("Long Data Table", tags$div(dataTableOutput('tblData'),style="height=600px")),
                          tabPanel('Reporter Table',tags$div(dataTableOutput('tblDataW'),style="height=600px")),
                          
                          tabPanel("Explore Plots",
                                        h4('Swap Faceting Variables'),
                                        uiOutput('tsFacetSwap'),
                                        hr(),
                                        h4('Click Plot Points for More Info'),
                                        dataTableOutput("plot_clickinfo"),
                                        plotOutput("timeSeriesPlot",click = 'tsClick'),
                                        hr(),
                                        h4('Click Plot Boxes for More Info'),
                                        dataTableOutput("bplot_clickinfo"),
                                        plotOutput("boxPlot",click = 'bxClick'),
                                        plotOutput("qPlot"),
                                        plotOutput("hPlot")
                          ),
                          tabPanel("Regression",
                                   
                                   fluidRow(
                                           h2('Regression Tools'),
                                           p('This tool allows for linear regression of 
                                             selected variables. Select the independent (x)
                                             variable and the dependent (y) variable.')
                                   ),
                                   fluidRow(
                                           column(3,uiOutput('regX')),
                                           column(3,uiOutput('regY'))
                                   ),
                                   fluidRow(
                                           
                                           plotOutput('rPlot')
                                   )
                                   
                                   
                                   ),
                          
                          tabPanel("Stats",
                                   
                                   fluidRow(
                                           
                                           column(6, h3('Select Stats to Show'),
                                                  actionLink('selectall_Stats','Select All | Clear All'),
                                                  uiOutput('choose_stats')),
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
   
        #Set options for larger file size import
        options(shiny.maxRequestSize=50*1024^2)
        
        #Data import-----------------------------------------
        #Demo data load-------------------
        
        
       
        #File input load--------------
        pData <- reactive({
                
                if(input$demoLoad == 0) {
                        inFile <- input$file
                }
                        
                if(input$demoLoad > 0){
                        
                        inFile<-data.frame(
                               name='demoData',
                               size=1413000,
                               type='csv',
                               datapath='./data/demoData.csv'
                        )
                        
                        inFile$datapath<-as.character(inFile$datapath)
                }
                
                
                if (is.null(inFile))
                        return(NULL)
                
                
                #Read in table
                tbl <- read.csv(inFile$datapath, header=TRUE, stringsAsFactors = FALSE)
                #Fix date format
                tbl$Date<-as.POSIXct(strptime(tbl$Date,format="%d-%b-%y"))
                #Fix unit cases
                tbl$Units<-fixUnits(tbl)
                #Add units to parameter column
                tbl$Parameter<-paste0(tbl$Parameter,' (',tbl$Units,')')
                #Make non detect substitution columns
                tbl$Result_ND<-as.numeric(ifelse(tbl$DetectionFlag=='ND',tbl$ReportingLimit*0.5,tbl$Value))
                tbl$NonDetect<-as.numeric(ifelse(tbl$DetectionFlag=='ND',tbl$ReportingLimit*0.5,''))
                
                return(tbl)
        })
        
        #Get min and max dates
        baseDates <- reactive({
                if(input$demoLoad == 0) {
                        inFile <- input$file
                }
                
                if(input$demoLoad > 0){
                        
                        inFile<-data.frame(
                                name='demoData',
                                size=1413000,
                                type='csv',
                                datapath='./data/demoData.csv'
                        )
                        
                        inFile$datapath<-as.character(inFile$datapath)
                }
                
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
        
        #Swap plot faceting variable picklist
        output$tsFacetSwap<-renderUI({
                pickerInput('tsSwap',choices = c('Parameter','Location'),
                                     selected='Parameter') 
        
        })
        
        #Pickers----------------------------------------------------
        #Create location picker
        #This loads up by default with no choices available
        output$choose_locs<-renderUI({
                
                 if(is.null(pData()))
                         return()
                 locs<-sort(unique(pData()$Location))
                
                checkboxGroupInput('locids',NULL,
                                   choices = locs,
                                   selected = NULL)
        })
        
        #THis listens for values selected in other checkbox groups and changes accordingly 
        observe({
                
                if(is.null(pData()))
                        return()
                lulocs<-pData() %>% filter(Site %in% input$sts,Matrix %in% input$mtrx)
                locs<-sort(unique(lulocs$Location))
                
                

                if(input$selectall_Locs == 0) 
                {
                        updateCheckboxGroupInput(session,"locids",NULL,choices=locs,selected=NULL)
                }
                else if (input$selectall_Locs%%2 == 0)
                {
                        updateCheckboxGroupInput(session,"locids",NULL,choices=locs)
                }
                else
                {
                        updateCheckboxGroupInput(session,"locids",NULL,choices=locs,selected=locs)
                }

        })

        
        #Create parameter picker
        observe({
                params<-sort(unique(pData()$Parameter))
                if(input$selectall_Params == 0) return(NULL) 
                else if (input$selectall_Params%%2 == 0)
                {
                        updateCheckboxGroupInput(session,"params",NULL,choices=params)
                }
                else
                {
                        updateCheckboxGroupInput(session,"params",NULL,choices=params,selected=params)
                }
        })
        output$choose_params<-renderUI({
                if(is.null(pData()))
                        return()
                params<-sort(unique(pData()$Parameter))
                
                checkboxGroupInput('params',NULL,
                                   choices = params)
        })
        
        #Create matrix picker
        observe({
                matrices<-sort(unique(pData()$Matrix))
                if(input$selectall_Matrix == 0) return(NULL) 
                else if (input$selectall_Matrix%%2 == 0)
                {
                        updateCheckboxGroupInput(session,"mtrx",NULL,choices=matrices,inline = FALSE)
                }
                else
                {
                        updateCheckboxGroupInput(session,"mtrx",NULL,choices=matrices,selected=matrices,inline = FALSE)
                }
        })
        
        output$choose_matrix<-renderUI({
                if(is.null(pData()))
                        return()
                matrices<-sort(unique(pData()$Matrix))

                checkboxGroupInput('mtrx',NULL,
                                   choices = matrices,
                                   selected = matrices[1],
                                   inline = FALSE)
        })
        
        #Create site picker
        observe({
                sites<-sort(unique(pData()$Site))
                if(input$selectall_Sites == 0) return(NULL) 
                else if (input$selectall_Sites%%2 == 0)
                {
                        updateCheckboxGroupInput(session,"sts",NULL,choices=sites,inline = FALSE)
                }
                else
                {
                        updateCheckboxGroupInput(session,"sts",NULL,choices=sites,selected=sites,inline = FALSE)
                }
        })
        
        output$choose_sites<-renderUI({
                if(is.null(pData()))
                        return()
                sites<-sort(unique(pData()$Site))
                
                checkboxGroupInput('sts',NULL,
                                   choices = sites,
                                   selected = sites[1],
                                   inline = FALSE)
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
        
        #Create stats table column picker-----------------
        observe({
                
                if(input$selectall_Stats == 0) return(NULL) 
                else if (input$selectall_Stats%%2 == 0)
                {
                        updateCheckboxGroupInput(session,"stats",NULL,choices=statList,inline = TRUE)
                }
                else
                {
                        updateCheckboxGroupInput(session,"stats",NULL,choices=statList,selected=statList,inline = TRUE)
                }
        })
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
        
        
        #Create regression tool parameter picker-----
        observe({
                
                #if(is.null(input$params)) return(NULL)
                
                #if (input$selectall_Params == 0) return(NULL)
                
                if (input$selectall_Params%%2 == 0)
                {
                        updatePickerInput(session,'regrX',choices='')
                        updatePickerInput(session,'regrY',choices='')
                }
                
                else if(input$selectall_Params%%2 == 1)
                {
                        updatePickerInput(session,'regrX',choices=c('Date',input$params),
                                          selected='Date')
                        updatePickerInput(session,'regrY',choices=input$params)
                } 
                
                
                
        })
        
        
        
        output$regX<-renderUI({
                pickerInput('regrX',choices=c('Date',input$params))
                
        })
        
        output$regY<-renderUI({
                pickerInput('regrY',choices=input$params)
                
        })
        
        
        
        #Data table output-------------------
        #Output for long format data table
        
        output$tblData <- renderDataTable({
                if(is.null(input$locids))
                        return()
                
                fData<-pData() %>% filter(Location %in% input$locids,Matrix %in% input$mtrx,
                                          Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                          Parameter %in% input$params, Site %in% input$sts) %>% 
                        arrange(Location,Date)
                fData$Date<-as.character(fData$Date)
                
                return(unique(fData))
        })
        
        #Output for wide data format table
        
        output$tblDataW<-renderDataTable({
                
                if(is.null(input$locids))
                        return()
                
                wData<-pData() %>% filter(Location %in% input$locids,Matrix %in% input$mtrx,
                                          Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                          Parameter %in% input$params, Site %in% input$sts) %>% 
                        arrange(Location,Date)
                wData$Date<-as.character(wData$Date)
                
                wData<-data.table::dcast(unique(wData),Parameter~Location+Date,fun.aggregate=max,value.var='Result_ND')
                
                
                
                return(wData)
                
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
        
        #Export data table download button------------------
        #Output handler
        output$expData <- downloadHandler(
                
                filename = function() {
                        paste(input$expDataFilename, ".csv", sep = "")
                },
                
                content = function(file) {
                        write.csv(statSumm(), file, row.names = FALSE)
                }
        )     
        
        #Plotting-----------------------------------------------
        #Explore plots--------------------------------
        
        
        #TODO ADD MATRIX CODE TO THE FILTER ARGUMENTS IN THE PLOTS
        #Timeseries plots------
        output$timeSeriesPlot <- renderPlot({
                if(is.null(input$locids))
                        return()
                
                if(input$tsSwap=='Parameter'){
                        tsFacet<-'Parameter'
                        col<- 'Location'
                        
                }
                if(input$tsSwap=='Location'){
                        tsFacet<-'Location'
                        col<- 'Parameter'
                        
                }
                
                
                tsData<-as.data.frame(filter(pData(),Location %in% input$locids,
                                             Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                             Parameter %in% input$params,
                                             Matrix %in% input$mtrx))
                #tsp<-tsPlot(tsData,tsFacet,col)

                #return(tsp)
                
                #testing ggplot login inside this instead of function
                ggplot(tsData,aes(x=Date,y=Result_ND))+
                        geom_line(aes_string(colour=col),size=0.5)+
                        geom_point(aes_string(colour=col),size=3)+
                        geom_point(aes(x=Date,y=NonDetect,fill='Non-Detect at 1/2 MDL'),shape=21,size=2)+
                        scale_fill_manual(values='white')+
                        facet_wrap(as.formula(paste('~',tsFacet)),scales="free")+
                        theme(legend.position = "bottom", legend.title = element_blank())+
                        theme(strip.background = element_rect(fill = '#727272'),strip.text = element_text(colour='white',face='bold',size = 12))+
                        labs(x="Date",y="Value",title="Time Series Non-Detects Hollow at 1/2 the Reporting Limit")+
                        theme(plot.title = element_text(face='bold',size=14))
                
                
                
        })
        
        tsData2<-reactive({
                filter(pData(),Location %in% input$locids,
                       Date >= input$dtRng[1] & Date<=input$dtRng[2],
                       Parameter %in% input$params,
                       Matrix %in% input$mtrx) %>% 
                        select(Site,Location,Date,Matrix,Parameter,DetectionFlag,Result_ND,ReportingLimit,MDL)
        })
        
        #Time series info boxes
        output$plot_clickinfo <- renderDataTable({
                
                #nearPoints(pData(), input$tsClick,threshold = 10)
                
                res <- nearPoints(tsData2(), input$tsClick,threshold = 3,maxpoints = 1)
                
                if (nrow(res) == 0)
                        return()
                res
        })
        
        
        
        #Boxplots------
        output$boxPlot <- renderPlot({
                if(is.null(input$locids))
                        return()
                
                if(input$tsSwap=='Parameter'){
                        bxFacet<-'Parameter'
                        bxCol<- 'Location'
                        
                }
                if(input$tsSwap=='Location'){
                        bxFacet<-'Location'
                        bxCol<- 'Parameter'
                        
                }
                
                
                
                
                bxData<-as.data.frame(filter(pData(),Location %in% input$locids,
                                             Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                             Parameter %in% input$params,
                                             Matrix %in% input$mtrx))
                # bxp<-bxPlot(bxData)
                # return(bxp)
                
                ggplot(bxData,aes(x=Location,y=Value))+
                        geom_boxplot(aes_string(fill=bxCol))+
                        #geom_jitter(color="black")+
                        #geom_jitter(aes(x=Location,y=NonDetect),color="white")+
                        facet_wrap(as.formula(paste('~',bxFacet)), scales="free")+
                        theme(strip.background = element_rect(fill = '#727272'),strip.text = element_text(colour='white',face='bold',size = 12))+
                        theme(legend.position = "bottom", legend.title = element_blank())+
                        labs(x="Location",y="Value",title="Boxplots Non-Detects at Zero")+
                        theme(plot.title = element_text(face='bold',size=14))
                
                
        })
        
        bxData2<-reactive({pData() %>% filter(Location %in% input$locids,
                                     Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                     Parameter %in% input$params,
                                     Matrix %in% input$mtrx) %>% 
                        group_by(Location,Parameter) %>% 
                        summarize(
                                
                                `Percent Non-Detect`=round(perND(DetectionFlag,'ND'),0),
                                `Minimum Date`=min(Date),
                                `Maximum Date`=max(Date),
                                Minimum=as.character(ifelse(min(Value)==0,'ND',min(Value))),
                                `First Quartile`=as.character(ifelse(quantile(Value,0.25)==0,'ND',quantile(Value,0.25))),
                                Mean=mean(Value),
                                Median=median(Value),
                                `Third Quartile`=as.character(ifelse(quantile(Value,0.75)==0,'ND',quantile(Value,0.75))),
                                Maximum=max(Value),
                                Value = mean(Value)   
                        ) %>% as.data.frame(.)
                })
        
        #Boxplot info box table
        output$bplot_clickinfo <- renderDataTable({
                
                #nearPoints(pData(), input$tsClick,threshold = 10)
                
                bres <- nearPoints(bxData2(), input$bxClick,threshold = 50,maxpoints = 1)
                
                if (nrow(bres) == 0)
                        return()
                bres
        })
        
        #QQ Plots------------
        output$qPlot <- renderPlot({
                if(is.null(input$locids))
                        return()
                qData<-as.data.frame(filter(pData(),Location %in% input$locids,
                                             Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                            Parameter %in% input$params,
                                            Matrix %in% input$mtrx))
                qp<-qPlot(qData)
                return(qp)
        })
        
        #Histograms-------------
        output$hPlot <- renderPlot({
                if(is.null(input$locids))
                        return()
                hData<-as.data.frame(filter(pData(),Location %in% input$locids,
                                            Date >= input$dtRng[1] & Date<=input$dtRng[2],
                                            Parameter %in% input$params,
                                            Matrix %in% input$mtrx))
                hp<-hPlot(hData)
                return(hp)
        })
        
        #Regression plot------------------------
        
        output$rPlot<-renderPlot({
                
                if(is.null(input$regrX)) return()
                
                rData<-pData() %>%
                        filter(Location %in% input$locids,
                               Date >= input$dtRng[1] & Date<=input$dtRng[2],
                               Parameter %in% input$params,
                               Matrix %in% input$mtrx) %>% 
                        select(Location,Date,Parameter,Result_ND) %>% 
                        arrange(Location,Parameter,Date) %>% 
                        as.data.frame(.)
                
                rX<-ifelse(
                        input$regrX=='Date',
                        as.vector(sort(rData$Date)),
                        rData %>% filter(Parameter == input$regrX) %>% 
                                pull(Result_ND)
                                
                )
                
                rY<- rData %>% filter(Parameter == input$regrY) %>% 
                                pull(Result_ND)
                
                rX<-sample(rX,10,replace=TRUE)
                rY<-sample(rY,10,replace=TRUE)
                
        #rY<-rnorm(100,5,2)
        #rX<-seq(1:100)              
               plot(rY~rX) 
        })
        
        
        
        
}

#-----------------App Component--------------------------------
shinyApp(ui = ui, server = server)

