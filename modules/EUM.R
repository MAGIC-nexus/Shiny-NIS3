EUMChoicesUI <- function(id){
  
  ns <- NS(id) 
  tagList(
    uiOutput(ns('scenario')),
    uiOutput(ns('scope')),
    uiOutput(ns('period')),
    uiOutput(ns('systems')),
    uiOutput(ns('show_Interfaces')),
    uiOutput(ns("FundInterface"))
  )
  
}


EUMPlotChoicesUI<-function(id){
  ns <- NS(id) 
  tagList(
    uiOutput(ns("indicator")),
    uiOutput(ns("level")),
    uiOutput(ns('period'))
  )
}



matrixEUM<-function(input,output,session,data){
 
  EUM<-reactive({
    data<-data()
    df<-filter(data, data$Scenario == input$scenario)
    eum <- filter(df, Interface %in% input$show_Interfaces,)
    funds <- input$FundInterface

    # TODO  el valor del fund debería ser el del sistema, en realidad, así que no debería haber problema..
    # quizás poner la opción a que se coja de un desplegable pero se puede pensar luego
    eumTotal<-data.frame('indicator' = as.character() ,
                         'Value' = as.numeric() ,
                         'Unit' = as.character(),
                         'indicator_Unit' = as.character(),
                         'Processor'= as.character(),
                         'Level'= as.character(),
                         'Flow' = as.character(),
                         'Fund'=as.character(),
                         'Subsystem' = as.character(),
                         'System' = as.character(),
                         'Scope' = as.character(),
                         'Period' = as.numeric(),
                         # 'Orientation' = as.character(),
                         stringsAsFactors = FALSE)
    if (length(funds!=0)){
      for (i in funds){
        tmp<-filter(df,df$Interface == i)
        eumTmp = data.frame(
          'indicator'= paste(eum$Interface,i,sep ="/"),
          'Value' =  eum$Value/tmp$Value[1],
          'Unit'=paste(eum$Unit,tmp$Unit[1],sep = "/"),
          'indicator_Unit' =  paste0(paste(eum$Interface,i,sep ="/"),"(",paste(eum$Unit,tmp$Unit[1],sep = "/"),")"),
          'Processor' = eum$Processor,
          'Level' = eum['Level'], # TODO pasa algo con esta columna
          'Flow' = eum$Interface,
          'Fund' = i,
          'Subsystem' = eum$Subsystem,
          'System' = eum$System,
          'Scope' = eum$Scope,
          'Period' = eum$Period,
          # 'Orientation' = eum$Orientation,
          stringsAsFactors = FALSE
        )
        eumTotal<-rbind(eumTotal,eumTmp,stringsAsFactors = FALSE)
      }
    }
    if ("Population" %in% names(input)){
      if (input$Population!=0){
        eumTmp = data.frame(
          'indicator'= paste(eum$Interface,"Population",sep ="/"),
          'Value' =  as.integer(eum$Value)/input$Population,
          'Unit'= paste(eum$Unit,"cap",sep = "/"),
          'indicator_Unit' = paste0(paste(eum$Interface,"Population",sep ="/"),"(",paste(eum$Unit,"cap",sep = "/"),")"),
          'Processor' = eum$Processor,
          'Level' = eum['Level'],
          'Flow' = eum$Interface,
          'Fund' = "Population",
          'Subsystem' = eum$Subsystem,
          'System' = eum$System,
          'Scope' = eum$Scope,
          'Period' = eum$Period,
          # 'Orientation' = eum$Orientation,
          stringsAsFactors = FALSE
        )
        eumTotal<-rbind(eumTotal,eumTmp,stringsAsFactors = FALSE)
      } 
      
    }
    
    eum<-eumTotal[!duplicated(eumTotal), ]
    
  }) 
  return(EUM)
}
   
    
   

batPlotEUMScope<-function(input, output, session, data){
  
  output$barPlot<-renderPlot({
    df<- data()
    dfplot<-filter(df, indicator == input$indicator, Level == input$level, Period == input$period, Scope != 'Total') #tabla con 1 indicador varios sistemas agrupados por externo e interno 
    StackedPlot(df = dfplot, Xcol = dfplot$Processor, Scope = dfplot$Scope)
  })
  
}

    
    
    
IndicatorsEUM<-function(input,output,session,data){    
     
    EUM<-reactive({
      eum<-data()
      eum<-filter(eum,System == input$systems, Scope == input$scope  , Period == input$period)
      eum<-eum[,!(colnames(eum) %in% c("indicator","Flow","Fund","Unit","System"))]%>%spread(indicator_Unit,value = Value)
     
    }) 
    
    return(EUM)
  }





