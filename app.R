#-----------------On Load Component---------------------------
library(shiny)
library(tidyverse)

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
              
              
              dateRangeInput("dates", label = h3("Select Date range"))
              
        ),
      
      # plot preview and data export tools
      mainPanel(
         tableOutput('plot.data')
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
        
        output$plot.data <- renderTable({
                if(is.null(input$locids))
                        return()
                fData<-filter(pData(),LOC_ID %in% input$locids)
                return(fData)
        })
        
}

#-----------------App Component--------------------------------
shinyApp(ui = ui, server = server)

