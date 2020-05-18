  

GaugeInputsUI<-function(id){
  
  
  ns<-NS(id)

  tagList(
    fluidRow(
      column(4,style=list("padding-right: 3px;"), 
             uiOutput(ns("indicator"))),
      
      column(4,style=list("padding-right: 3px;"),
             uiOutput(ns("level"))),
      
      column(4,style=list("padding-right: 3px;"),
             uiOutput(ns("scope"))),
      
      column(4,style=list("padding-right: 3px;"),
             uiOutput(ns("period")))
    ),
    
    fluidRow(
      column(4,style=list("padding-right: 3px;"),
             textInput(ns("IndName"), "Ind Name",placeholder = "Ind Name")),
      column(4,style=list("padding-right: 3px;"),
             selectInput(ns("BechmarkGroup"),"Bechmark Group", choices = c("Feasibility", "Viability", "Desirability",""), selected = "")),
      column(4,style=list("padding-right: 3px;"),
             textInput(ns("Description"), "Description",placeholder = "Short description"))
      
    ),
    
    h3("Zone 1"),
    #ZONE 1
    fluidRow(
      column(6,style=list("padding-right: 3px;"),
             textInput(ns("ZoneName1"), "Category",placeholder = "Low")),
      
      column(6,style=list("padding-right: 3px;"),
             selectInput(ns("colour1"),"colour", choices = c("red","gold","forestgreen"), selected = "red")), 
      
      column(3,style=list("padding-right: 3px;"),
             numericInput(ns("break1"), "Min", 0)),
      
      column(6,style=list("padding-right: 3px;"),
             numericInput(ns("break2"), "break 2", 0.1)),
      
      column(3,style=list("padding-right: 3px;"),
             selectInput(ns("Include1"), "",choices = c(")","]"),selected = "("))
    ),
    
    br(),
    h3("Zone  2"),
    fluidRow(
      column(6,style=list("padding-right: 3px;"),
             textInput(ns("ZoneName2"), "Category",placeholder = "Medium")),
      
      column(6,style=list("padding-right: 3px;"),
             selectInput(ns("colour2"),"colour", choices = c( "red","gold","forestgreen"),selected = "gold")), 
      
      column(3,style=list("padding-right: 3px;"),
             selectInput(ns("Include2"),"",choices = c("(","["),selected = "(")),
      
      column(6,style=list("padding-right: 3px;"),
             numericInput(ns("break3"), "break3", 2)),
      
      
      column(3,style=list("padding-right: 3px;"),
             selectInput(ns("Include3"),"",choices = c(")","]"),selected = "]"))
    ),
    
    p(), #TODO arreglar línea
    h3("Zone  3"),
    fluidRow(
      column(6,style=list("padding-right: 3px;"),
             textInput(ns("ZoneName3"), "Category",placeholder = "Max")),
      
      column(6,style=list("padding-right: 3px;"),
             selectInput(ns("colour3"),"colour", choices = c( "red","gold","forestgreen"),selected = "forestgreen")),
      
      column(6,style=list("padding-right: 3px;"),
             selectInput(ns("Include4"), "",choices = c("(","["),selected = "(")),
      
      column(6,style=list("padding-right: 3px;"),
             numericInput(ns("break4"), "Max", 2.5))
      
    )
  
  )

}








gaugePlotServer<-function(input,output,session,data)
  
  output$gaugePlot <- renderPlot({

    eum <- data()
    eumindicator<-filter(eum,eum$Level == input$level,indicator == input$indicator, eum$Scope == input$scope, eum$Period == input$period)
    gg.gauge(eumindicator,breaks = c(input$break1,input$break2,input$break3,input$break4), colour = c(input$colour1,input$colour2,input$colour3))
    
  }, height = 400, width = 800 ) #this one seems to not change anything






Benchmarks<-function(input,output,session){

  ScalarBenchmarks<-reactive({
    data.frame(
      'benchMarkGroup' = rep(input$BechmarkGroup,3),
      'Stakeholders' = rep('',3),
      'Range' = c(paste0("[",input$break1,',',input$break2,input$Include1),
                  paste0(input$Include2,input$break2,',',input$break3,input$Include3),
                  paste0(input$Include4,input$break3,',',input$break4,']')),
      'Category' = c(input$ZoneName1,input$ZoneName2, input$ZoneName3),
      'Label' = c(input$ZoneName1,input$ZoneName2, input$ZoneName3)
    )    
    
  })

}




Indicators<-function(input,output,session){

  ScalarIndicators<-reactive({
    if ( input$scope == 'Local'){
      Local = 'Yes'
    }
    else{
      Local = "No"
    }
    data.frame(
      'Indicator' = input$IndName,
      'Local'= Local,
      'Formula' = input$Indicators,
      'Banchmark' = 'b1',
      'Description' = input$Description
      # TODO crear un contador para que vaya agregando los bechmakr
    )
  })
  
}







  
  
  