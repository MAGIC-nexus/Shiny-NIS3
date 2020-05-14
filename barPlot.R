

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
  
  
barPlotUI <- function(id,stringName){
  ns <- NS(id) 
  tagList(
    plotOutput(ns(stringName))
             )
}




barPlotChoices <- function(input, output, session, data){  
  
  output$scenario = renderUI({
    datos<-data()
    Scenarios <- as.vector(unique(datos$Scenario))
    selectInput(session$ns('scenario'), "Choose a Scenario:", Scenarios)
  
  })
  
  
    
    output$period = renderUI({
      datos<-data()
      datos$Period<-as.numeric(datos$Period)
      Periods<- as.vector(unique(datos$Period))
      selectInput(session$ns('period'),  "Choose a Period:", Periods, selected = 2017)
    })
    
    
    output$level = renderUI({
      datos<-data()
      Level <-as.vector(unique(datos$Level))
      selectInput(session$ns('level'), "Choose a level:", Level)
    })
    
}
  


barPlotChoicesScope <- function(input, output, session, data){

output$scope = renderUI({
  datos<-data()
  Scopes<- as.vector(unique(datos$Scope))
  selectInput(session$ns('scope'), "Choose a Scope:", Scopes)
})}


  
barPlotChoicesMultiInterface <- function(input, output, session, data){

    output$Interfaces = renderUI({
      datos<-data()
      Interfaces<- as.vector(unique(datos$Interface))
      checkboxGroupInput(session$ns("Interfaces"), "InterfacesTypes:",
                         choiceNames = Interfaces,choiceValues = Interfaces,  selected = Interfaces[1])

    })
}



barPlotChoicesInterface <- function(input, output, session, data){
  
  output$Interfaces = renderUI({
    datos<-data()
    Interfaces<- as.vector(unique(datos$Interface))
    selectInput(session$ns("Interfaces"), "InterfacesTypes:", Interfaces)
  })
}
    
    

barPlotChoicesMultiProcessor<- function(input, output, session, data){
  
  
  output$Processors = renderUI({
    datos<-data()
    Processors<- as.vector(unique(datos$Processor))
    checkboxGroupInput(session$ns("Processors"), "Processors to compare:",
                       choiceNames = Processors, choiceValues = Processors, selected = Processors[1])
  })
}





  MultibarPlotServerScope <- function(input, output, session, data){
    
    output$barPlot <- renderPlot({
      #TODO  % Value
      # TODO nombre de los processors en vertical
      data<-data()
      df <- filter(data,data$Scenario == input$scenario & data$Period == input$period & data$Level == input$level, data$Scope != 'Total')
      df <- filter(df, Interface %in% input$Interfaces, )
      validate(
        need(length(df)>0, "There is no data for your selection") 
      )
      StackedplotBarExtInt(df)
    })
    
  }
  
  
  barPlotServerScope <- function(input, output, session, data){
    
    output$barPlot <- renderPlot({
      data<-data()
      df <- filter(data,data$Scenario == input$scenario & data$Period == input$period & data$Level == input$level, data$Scope != 'Total', data$Interface == input$Interfaces)
      browser()
      validate(
        need(length(df)>0, "There is no data for your selection") 
      )
      StackedplotBarExtInt(df)
    })
  }
  
  
  
  
  
  MultibarPlotServer <- function(input, output, session, data){
    
    output$barPlot <- renderPlot({
      #TODO  % Value
      # TODO nombre de los processors en vertical
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
        need(length(df)>0, "There is no data for your selection") 
      )
      StackedplotBar(df)
    })
  }
  

  
  ScenarioTimeChoice<-function(input, output, session, data){
    
    output$scenario = renderUI({
      datos<-data()

      Scenarios <- as.vector(unique(datos$Scenario))
      selectInput(session$ns('scenario'), "Choose a Scenario:", Scenarios)
      
    })
    
    
    
    output$period = renderUI({
      datos<-data()
      datos$Period<-as.numeric(datos$Period)
      Periods<- as.vector(unique(datos$Period))
      selectInput(session$ns('period'),  "Choose a Period:", Periods, selected = 2017)
    })
    
  }  
  
  
  barPlotSubsystemServer <- function(input, output, session, data){ 
# TODO hay algún fallo con eso  
    output$barPlot <- renderPlot({
      data<-data()

      df <- filter(data, data$Scenario == input$scenario & data$Period == input$period & data$Level == "Subsystem" & data$Interface == input$Interfaces)

      validate(
        need(length(df)>0, "There is no data for your selection") 
      )

     browser()
      barchart <- ggplot (df, aes( x = System ,  y = Value, fill = Subsystem)) + 
        geom_bar( position="stack", stat = "identity") +
        labs(title = "Inrterface value", y = unique(df$Unit)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    })
    
  }
  
  
  
  barPlotProcessorInterfaceSertver <- function(input, output, session, data){

    output$PiePlotProcessors <- renderPlot({
      data<-data()
      df <- filter(data,data$Scenario == input$scenario &  data$Period == input$period, data$Scope == input$scope)
      df <- filter(df, Processor %in% input$Processors, )
      df <- filter(df, Interface %in% input$Interfaces, )
      browser()

      validate(
        need(length(df)>0,"There is no data for your selection")
      )

    StackedplotBar(df)

    })

}
  
  
  

  
