function(input, output,session) {
  
  #close session and windows
 
  #log out when close the windows
  session$onSessionEnded(function() {
    print(c$close_session())
    print(c$logout())
    stopApp()
  })

  # INPUT FILE TO NIS ----  

  
  #INPUT FGS XLSX ----
  NisOutput<- reactive({
    df <- pandas$read_csv('flow_graph_solution_multi_system.csv')
    df$Period<-as.character(df$Period)
    list(df1 = df, df2 = df)
  })

  output$FGS<- renderDataTable({
    df <- NisOutput()[['df1']]
    DT::datatable(df)
  })

  output$issues<- renderDataTable({
    df <- NisOutput()[['df2']]
    DT::datatable(df)
  })
  
  
  # Reactive FGS Absolute values ------
  dfAbs<-reactive({
    df<-NisOutput()[['df1']]
    
    #Cleaing
    df<-df %>% replace_na(list(Level = 'Subsystem'))
    df[is.na(df)] <- "Subsystem"
    
    
    # Select oly useful data for analysis
    df<- filter(df, Conflict_Partof != 'Dismissed', Conflict_Itype != 'Dismissed')
    cols<- c('Scenario','Period','Scope','Processor','Interface','Orientation','RoegenType','Value','Unit','Level','System','Subsystem','Sphere')
    df<-subset(df,select = cols)
    
    #Spreading Value by orientation
    data_spread<-df%>%spread(Orientation,value = Value)
    data_spread<-data_spread %>% replace_na(list(Input = 0, Output = 0))
    data_spread['Value'] <- abs(data_spread['Input']-data_spread['Output'])
    data_spread
  })
 
  
  # Reactive FGS Negative Output ------
    dfIO <- reactive({
      data_spread<-dfAbs()
      data_spread['Output']<- data_spread['Output']*-1
      data_gather<-data_spread%>%gather(key = "Orientation", value= "Value", Input, Output, na.rm = TRUE)
    })
  
  
  df<-callModule(choosedf,'choose',dfAbs,dfIO)
 
  
  
  
  
  # # BAR PLOT Scope choice grouped   ---------------------
  # 
  # 
  # df<-callModule(choosedf,'first',dfAbs,dfIO)
  # callModule(ChoicesSPL, "first", df)
  # callModule(ChoicesScope,"first",df)
  # callModule(ChoicesMultiInterface,'first',df)
  # callModule(MultibarPlotServer,"first",df)
  #   
  # BAR PLOT SCOPES stacked  Interface grouped  ---------------------
  #TODO leyend
  # Interface grouped
  df<-callModule(choosedf,'Scope',dfAbs,dfIO)
  callModule(ChoicesSPL, "Scope", df)
  callModule(ChoicesMultiInterface,'Scope',df)
  callModule(MultibarPlotServerScope,"Scope",df)
  
  #  BAR CHART BY SYSTEM stacked subsystems  ----
  df<-callModule(choosedf,'System',dfAbs,dfIO)
  callModule(ScenarioTimeChoice,'System',df)
  callModule(ChoicesInterface,'System',df)
  callModule(barPlotSubsystemServer,'System',df)
  
  
  
  # BAR CHART BY PROCESSOR AND INTERFACE choice  ------
  # Reactive Inputs:
  df<-callModule(choosedf,'System',dfAbs,dfIO)
  callModule(ScenarioTimeChoice, "processor", df)
  callModule(ChoicesScope,"processor",df)
  callModule(ChoicesMultiInterface,'processor',df)
  callModule(ChoicesMultiProcessor,'processor',df)
  callModule(barPlotProcessorInterfaceSertver,'processor',df)
  

  
  
  # new Reactive EUM (all indicators in the same column) ----
  

  
  #eum formato corto ----
    
  # EUM -----
  #Reactive Inputs
  
  


  callModule(ChoicesScenario,'EUM',dfAbs)
  callModule(ChoicesPeriod,'EUM',dfAbs)
  callModule(ChoicesSystem,'EUM',dfAbs)
  callModule(ChoicesScope,'EUM',dfAbs)
  callModule(ChoicesFlow,'EUM',dfAbs)
  callModule(ChoicesFund,'EUM',dfAbs)

  
  totalEUM<-callModule(EUM,'EUM',dfAbs)
  ShortEUM<-callModule(IndicatorsEUM,'EUM',totalEUM)


  
  
  #EUM output en formato excel ----
  
  output$eum<-renderExcel({
    excelTable(data = ShortEUM())
  })

  

  
  
  #PLOT Indicators ----
  #inputs
  
  callModule(ChoicesIndicator,'EUMplot',totalEUM)
  callModule(ChoicesLevel,'EUMplot',totalEUM)
  callModule(ChoicesPeriod,'EUMplot',totalEUM)
  callModule(batPlotEUMScope,'EUMplot',totalEUM)

  
  
  # Temporal Study of Indicators ----
  output$ind1 = renderUI({
    df <- totalEUM()
    ind <- unique(df$indicator_Unit)
    selectInput("ind1", "Choose a indicator X axis:",
                choices = ind, selected = ind[1])
    
  })
  
  output$ind2 = renderUI({
    df <- totalEUM()
    ind <- unique(df$indicator_Unit)
    selectInput("ind2", "Choose a indicator Y axis:",
                choices = ind, selected = ind[2])
    
  })
  output$ind3 = renderUI({
    df <- totalEUM()
    ind <- unique(df$indicator_Unit)
    selectInput("ind3", "Choose a indicator size dimmension:",
                choices = ind, selected = ind[3])
  })
  
  #Plot temporal
  
  output$temporal<-renderPlot({
    eum<-totalEUM()
    eum<-filter(eum, Scope == input$ScopeChoice, Level == input$level)
    # eum$ID <-seq.int(nrow(eum)) #TODO esto es una chanada, las filas no se deberían repetar
    eum<-eum[,!(colnames(eum) %in% c("Flow","Fund","Unit","indicator"))]%>%spread(indicator_Unit,value = Value)
    ggplot(eum, aes(eum$input$ind1 , eum$input$ind2)) +
      geom_point(aes(colour =  factor( System), size = eum[input$ind3], alpha = Period))
  })
  
 
  
   # Gauge tab ---
  
  callModule(ChoicesIndicator,'gauge',totalEUM)
  callModule(ChoicesLevel,'gauge',totalEUM)
  callModule(ChoicesPeriod,'gauge',totalEUM)
  callModule(ChoicesScope,'gauge',totalEUM)
  callModule(gaugePlotServer,'gauge',totalEUM)
  
  
  
  #Create command -----
  
  ScalarBenchmarks<-callModule(Benchmarks,'gauge')
  ScalarIndicators<-callModule(Indicators,'gauge')
  

 
  # Add command   
  observeEvent(input$addCommands,{
    c = NisOutput()[['c']]
    c$check_backend_available()
    c$append_command('ScalarIndicators',ScalarIndicators())
    c$append_command('ScalarBenchmarks',ScalarBenchmarks())
  # TODO ir haciendo el append
    })
  
  # download Commands xlsx
 
  output$dl <- downloadHandler(
    filename = function() {"Indicators.xlsx"},
    content = function(file) {write_xlsx(list(ScalarIndicators =ScalarIndicators(), ScalarBenchmarks = ScalarBenchmarks()), path = file)}
    )
   
  
  
  # TREE WITH QUANTITIES -----

  
  #tree Output ----
  output$Tree<-renderCollapsibleTree({
    if (input$act==0)
      return()
    #    isolate({
    data<-NisOutput()[['df1']]
    Level <-as.vector(unique(data$Level))
    datafilter<-filter(data,data$Scope == input$ScopeTree, data$Period == input$PeriodTree)
    tree<-datafilter%>%separate(Processor,c(Level), sep= "\\.")
    collapsibleTree(df = tree, c(Level), fill = "green", width = 800)
    
    #    })
  })
  
  
  
  # TREE ----
  # REACTIVE INPUTS
  
  callModule(ChoicesScope,'tree',dfAbs)
  callModule(ChoicesPeriod,'tree',dfAbs)
  callModule(ChoicesInterface,'tree',dfAbs)
  callModule(TreeServer,'tree',dfAbs)
  
  
  
  output$Unit <- renderText({
    if (input$act==0)
      return()
    data<- NisOutput()[['df1']]
    Unit<- paste("Unit",filter(data,data$Interface == input$InterfaceTree2)$Unit[1],sep = "=")
    
  })  
  
} #END