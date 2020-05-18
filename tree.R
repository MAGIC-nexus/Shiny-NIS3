
ChoicesTreeUI <- function(id){
  
  ns <- NS(id) 
  tagList(
  uiOutput(ns("scope")),
  uiOutput(ns("period")),
  uiOutput(ns("Interfaces"))
  )
}

treeUI<-function(id,StringName){
  
  ns <- NS(id) 
  collapsibleTreeOutput(ns("TreeInterface"))
}


TreeServer <- function(input, output, session, data){
  output$TreeInterface<-renderCollapsibleTree({
    data<-data()

    data<-filter(data,Level != '')
    Level <-as.vector(unique(data$Level))
    Level<-as.vector(unlist(Level,use.names = FALSE))
    datafilter<-filter(data,data$Scope == input$scope,data$Period == input$period, data$Interface == input$Interfaces)
    treePlot(datafilter, levelList = Level)
  })
}