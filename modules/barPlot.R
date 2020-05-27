
barPlotChoicesUI <- function(id){
  
  ns <- NS(id) 
  tagList(
    uiOutput(ns('scenario')),
    uiOutput(ns('scope')),
    uiOutput(ns('period')),
    uiOutput(ns('level')),
    uiOutput(ns("Interfaces"))
  )
  
}


barPlotChoicesMultiProcessors<-function(id){
  ns <- NS(id) 
  tagList(
  
    uiOutput(ns('scenario')),
    uiOutput(ns('scope')),
    uiOutput(ns('period')),
    fluidRow(
      column(6,style=list("padding-right: 3px;"), 
             uiOutput(ns("Interfaces"))),
      column(6,style=list("padding-right: 3px;"), 
             uiOutput(ns("Processors")))
    )
  )
}


barPlotUI <- function(id,stringName){
  ns <- NS(id) 
  tagList(
    plotOutput(ns(stringName), height = 1000, width = 1000)
  )
}


  MultibarPlotServerScope <- function(input, output, session, data){
    
    output$barPlot <- renderPlot({
      #TODO  % Value
      # TODO nombre de los processors en vertical
      data<-data()

      df <- filter(data,data$Scenario == input$scenario & data$Period == input$period & data$Level == input$level, data$Scope != 'Total')
      df <- filter(df, Interface %in% input$Interfaces, )
      validate(
        need(nrow(df)>0, "There is no data for your selection") 
      )
      dodgefacetsbarScopes(df)
      
    })
    
    
  }
  
  
  
  barPlotSubsystemServer <- function(input, output, session, data){ 
    output$barPlot <- renderPlot({
      data<-data()

      dfplot <- filter(data, data$Scenario == input$scenario & data$Period == input$period & data$Level == "Subsystem" & data$Interface == input$Interfaces & Scope != 'Total')

      validate(
        need(nrow(dfplot)>0, "There is no data for your selection") 
      )
      

      StackedPlot(df = dfplot, Xcol = dfplot$System, Scope = dfplot$Scope)
      
    })
    
  }
  
  
  
  barPlotProcessorInterfaceSertver <- function(input, output, session, data){

    output$barPlot <- renderPlot({
      data<-data()
      df <- filter(data,data$Scenario == input$scenario &  data$Period == input$period, data$Scope == input$scope)
      df <- filter(df, Processor %in% input$Processors, )
      df <- filter(df, Interface %in% input$Interfaces, )
      validate(
        need(nrow(df)>0,"There is no data for your selection")
      )
      
    StackedplotBars(df)

    })
  }
  
 
 

  
  

  
