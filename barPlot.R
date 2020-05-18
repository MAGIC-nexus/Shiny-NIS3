
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
    plotOutput(ns(stringName))
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
      StackedplotBarExtInt(df)
    })
    
  }
  
  
  barPlotServerScope <- function(input, output, session, data){
    
    output$barPlot <- renderPlot({
      data<-data()
      df <- filter(data,data$Scenario == input$scenario & data$Period == input$period & data$Level == input$level, data$Scope != 'Total', data$Interface == input$Interfaces)

      validate(
        need(nrow(df)>0, "There is no data for your selection") 
      )
      StackedplotBarExtInt(df)
    })
  }
  
  
  
  
  
  MultibarPlotServer <- function(input, output, session, data){
    
    output$barPlot <- renderPlot({

      data<-data()
      df <- filter(data,data$Scenario == input$scenario & data$Period == input$period & data$Level == input$level, data$Scope == input$scope)
      df <- filter(df, Interface %in% input$Interfaces, )
      validate(
        need(length(df)>0, "There is no data for your selection") 
      )
      StackedplotBar(df)
    })
    
  }
  
  
  barPlotServer <- function(input, output, session, data){
    
    output$barPlot <- renderPlot({
      data<-data()
      df <- filter(data,data$Scenario == input$scenario & data$Period == input$period & data$Level == input$level, data$Scope == input$scope, data$Interface == input$Interfaces)
      validate(
        need(nrow(df)>0, "There is no data for your selection") 
      )
      StackedplotBar(df)
    })
  }
  
  
  
  barPlotSubsystemServer <- function(input, output, session, data){ 
# TODO hay algún fallo con eso  
    output$barPlot <- renderPlot({
      data<-data()

      df <- filter(data, data$Scenario == input$scenario & data$Period == input$period & data$Level == "Subsystem" & data$Interface == input$Interfaces & Scope != 'Total')

      validate(
        need(nrow(df)>0, "There is no data for your selection") 
      )
      barchart <- ggplot (df, aes( x = System ,  y = Value, fill = Scope)) + 
        geom_bar( position="stack", stat = "identity") +
        labs(title = "Inrterface value", y = unique(df$Unit)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      barchart
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
      
    StackedplotBar(df)

    })

  }
  
  
 batPlotEUMScope<-function(input, output, session, data){
   
   output$barPlot<-renderPlot({
     df<- data()
     dfplot<-filter(df, indicator == input$indicator, Level == input$level, Period == input$period, Scope != 'Total') #tabla con 1 indicador varios sistemas agrupados por externo e interno 
    StackedPlot(df = dfplot, Xcol = dfplot$Processor, Scope = dfplot$Scope)
   })
   
 }
 
 
 
 


  
  
  

  
