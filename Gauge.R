

gaugeInputs<- function(id,breakn,ZoneNamen,zoneColor,){
  ns<-NS(id)
  colors<-c( "red","gold","forestgreen")
  close<- c("(","[")
  open<-c(")","]")
  tagList(
    
    column(6,style=list("padding-right: 3px;"),
           textInput("ZoneName3", "Category",placeholder = "Max")),
    
    column(6,style=list("padding-right: 3px;"),
           selectInput("colour3","colour", choices =colors ,selected = zoneColor)),
    
    column(6,style=list("padding-right: 3px;"),
           selectInput("Include4", "",choices = close ,selected = "(")),
    
    column(6,style=list("padding-right: 3px;"),
           numericInput("break4", "Max", 2.5))  
    
  )
  
  }