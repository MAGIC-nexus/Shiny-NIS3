ChoosedfUI<-function(id){
  ns<-NS(id)
  checkboxInput(ns("Absolute"), label = "See Absolute Values", value = TRUE)
}



choosedf<-function(input,ouput,session,dfAbs,dfIO){
  df<-reactive({
    if (input$Absolute == TRUE){
      df<-dfAbs()
    }else{
      df<-dfIO()
    }
  })
  return(df)  
}



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




ChoicesSPL <- function(input, output, session, data){  
  
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


ChoicesScenario <- function(input, output, session, data){  #eee
  
  output$scenario = renderUI({
    datos<-data()
    Scenarios <- as.vector(unique(datos$Scenario))
    selectInput(session$ns('scenario'), "Choose a Scenario:", Scenarios)
    
  })
}


ChoicesPeriod <- function(input, output, session, data){  #eee
  
 
  
  output$period = renderUI({
    datos<-data()
    datos$Period<-as.numeric(datos$Period)
    Periods<- as.vector(unique(datos$Period))
    selectInput(session$ns('period'),  "Choose a Period:", Periods, selected = 2017)
  })

}


ChoicesLevel <- function(input, output, session, data){

  output$level = renderUI({
    datos<-data()
    Level <-as.vector(unique(datos$Level))
    selectInput(session$ns('level'), "Choose a level:", Level)
  })

}

ChoicesScope <- function(input, output, session, data){
  
  output$scope = renderUI({
    datos<-data()
    Scopes<- as.vector(unique(datos$Scope))
    selectInput(session$ns('scope'), "Choose a Scope:", Scopes)
  })
  
}



ChoicesMultiInterface <- function(input, output, session, data){
  
  output$Interfaces = renderUI({
    datos<-data()
    Interfaces<- as.vector(unique(datos$Interface))
    checkboxGroupInput(session$ns("Interfaces"), "InterfacesTypes:",
                       choiceNames = Interfaces,choiceValues = Interfaces,  selected = Interfaces[1])
    
  })
}



ChoicesInterface <- function(input, output, session, data){
  
  output$Interfaces = renderUI({
    datos<-data()
    Interfaces<- as.vector(unique(datos$Interface))
    selectInput(session$ns("Interfaces"), "InterfacesTypes:", Interfaces)
  })
}




ChoicesSystem<-function(input, output, session, data){
  
  output$systems = renderUI({
    data<-data()
    Systems<- as.vector (unique(data$System))
    selectInput(session$ns("systems"), "Choose a System:",
                choices = Systems, selected = Systems[1])
  })

  }


ChoicesMultiProcessor<- function(input, output, session, data){
  
  
  output$Processors = renderUI({
    datos<-data()
    Processors<- as.vector(unique(datos$Processor))
    checkboxGroupInput(session$ns("Processors"), "Processors to compare:",
                       choiceNames = Processors, choiceValues = Processors, selected = Processors[1])
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

ChoicesFund<-function(input, output, session, data){

  output$FundInterface = renderUI({
    data<- data()
    Fund<- filter(data, data$RoegenType == "Fund" | data$RoegenType == "fund")
    FundInterfaces <- as.vector(unique(Fund$Interface))
    checkboxGroupInput(session$ns("FundInterface"), "Choose a Fund InterfaceType:",
                       choices = FundInterfaces,selected = FundInterfaces[1])
  }) 

}

ChoicesFlow<-function(input, output, session, data){

  output$show_Interfaces = renderUI({
    data<- data()
    Flow<- filter(data, data$RoegenType == "flow" | data$RoegenType == "Flow")
    FlowInterfaces<- as.vector(unique(Flow$Interface))
    checkboxGroupInput(session$ns("show_Interfaces"), "Choose a flow InterfaceType to show:",
                       choiceNames = FlowInterfaces, choiceValues = FlowInterfaces, selected = FlowInterfaces[1])
  })
}

ChoicesIndicator<-function(input, output, session, data){

  output$indicator = renderUI({
    df <- data()
    ind <- unique(df$indicator)
    selectInput(session$ns("indicator"), "Choose a indicator:",
                choices = ind)
    
  })

}








