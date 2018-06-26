#-----------------On Load Component---------------------------
#Libraries
library(shiny)
library(tidyverse)

#Source functions
source('helpers.R')

#-----------------UI Component--------------------------------
ui <- fluidPage(
   
   # Application title
   titlePanel("Plotting App"),
   
   # Data input and plot parameter tools on left
   sidebarLayout(
        sidebarPanel(
         
              fileInput("file", label = h3("File input"),accept='.csv'),
              
              fluidRow(column(4, verbatimTextOutput("value"))),
              
              h3("Plot Configuration"),
              
              uiOutput('choose_locs'),
              
              uiOutput('choose_dates'),
              actionButton('clrDates','Reset',width='60px')
              
              
              
        ),
      
      # plot preview and data export tools
      mainPanel(
         
              tabsetPanel(type = "tabs",
                          tabPanel("Table", tags$div(dataTableOutput('tblData'),style="height=600px")),
                          tabPanel("Explore Plots", 
                                   fluidRow(
                                           column(6,plotOutput("timeSeriesPlot")),
                                           column(6,plotOutput("boxPlot"))
                                           )
                                ),
                          tabPanel("Multiple Plots","Coming Soon"),
                          tabPanel("Stats", dataTableOutput('statsData'))
              )
                    
      )
   )
)

#-----------------Server Component----------------------------
server <- function(input, output) {
   
        pData <- reactive({
                
                inFile <- input$file
                
                if (is.null(inFile))
                        return(NULL)
                
                tbl <- read.csv(inFile$datapath, header=TRUE, stringsAsFactors = FALSE)
                tbl$LOG_DATE<-as.POSIXct(strptime(tbl$LOG_DATE,format="%d-%b-%y"))
                
                return(tbl)
        })
        
        
        output$choose_locs<-renderUI({
                if(is.null(pData()))
                        return()
                locs<-unique(pData()$LOC_ID)
                
                checkboxGroupInput('locids',"Choose Locations",
                                   choices = locs,
                                   selected = locs)
        })
        
        output$choose_dates<-renderUI({
                if(is.null(pData()))
                        return()
                minDate<-min(pData()$LOG_DATE)
                maxDate<-max(pData()$LOG_DATE)
                
                dateRangeInput('dtRng',"Choose Date Range",
                               start = minDate,
                               end = maxDate,
                               format = 'dd-M-yyyy'
                                
                               )
        })
        
        output$tblData <- renderDataTable({
                if(is.null(input$locids))
                        return()
                
                fData<-pData() %>% filter(LOC_ID %in% input$locids,
                                          LOG_DATE >= input$dtRng[1] & LOG_DATE<=input$dtRng[2]) %>% 
                        arrange(LOC_ID,LOG_DATE)
                fData$LOG_DATE<-as.character(fData$LOG_DATE)
                
                return(fData)
        })
        
        output$timeSeriesPlot <- renderPlot({
                if(is.null(input$locids))
                        return()
                tsData<-as.data.frame(filter(pData(),LOC_ID %in% input$locids,
                                             LOG_DATE >= input$dtRng[1] & LOG_DATE<=input$dtRng[2]))
                tsp<-tsPlot(tsData,tsData$LOG_DATE,tsData$WATER_ELEV)
                return(tsp)
        })
        
        output$boxPlot <- renderPlot({
                if(is.null(input$locids))
                        return()
                bxData<-as.data.frame(filter(pData(),LOC_ID %in% input$locids,
                                             LOG_DATE >= input$dtRng[1] & LOG_DATE<=input$dtRng[2]))
                bxp<-bxPlot(bxData,bxData$LOC_ID,bxData$WATER_ELEV)
                return(bxp)
        })
        
        output$statsData <- renderDataTable({
                pData() %>% filter(LOC_ID %in% input$locids,
                                   LOG_DATE >= input$dtRng[1] & LOG_DATE<=input$dtRng[2]) %>% 
                        group_by(LOC_ID) %>% 
                        summarise(`Number of Observations`=length(WATER_ELEV),
                                  `Minimum Date`=min(LOG_DATE),
                                  `Maximum Date`=max(LOG_DATE),
                                  Minimum=min(WATER_ELEV),
                                  Maximum=max(WATER_ELEV),
                                  `First Quartile`=quantile(WATER_ELEV,0.25),
                                  `Third Quartile`=quantile(WATER_ELEV,0.75),
                                  Average=mean(WATER_ELEV),
                                  `Standard Deviaton`=sd(WATER_ELEV),
                                  Variance=var(WATER_ELEV))
        })
        
}

#-----------------App Component--------------------------------
shinyApp(ui = ui, server = server)

