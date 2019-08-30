#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
data <- mtcars

shinyApp(
        ui = fluidPage(
                selectInput("select1", "select cyl:", choices = unique(data$cyl)),
                uiOutput("checkbox"),
                dataTableOutput("table")
        ),
        server = function(input, output) {
                
                output$checkbox <- renderUI({
                        choice <-  unique(data[data$cyl %in% input$select1, "gear"])
                        checkboxGroupInput("checkbox","Select gear", choices = choice, selected = choice[1])
                        
                })
                
                output$table <- renderDataTable({
                        data <-  data %>% filter(cyl %in% input$select1) %>% filter(gear %in% input$checkbox)
                        datatable(data)
                })
                
        }
)