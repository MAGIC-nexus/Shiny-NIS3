function(input, output,session) {
  
  #close session and windows
 
  #log out when close the windows
  session$onSessionEnded(function() {
    print(c$close_session())
    print(c$logout())
    stopApp()
  })

  # # INPUT FILE TO NIS ----  


  # INPUT FGS XLSX for faster testing ----
  # NisOutput<- reactive({
  #   df <- pandas$read_csv('flow_graph_solution.csv')
  #   df$Period<-as.character(df$Period)
  #   list(df1 = df, df2 = df)
  # })
  
  NisOutput<-callModule(NisOutput,'input')
  

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
    
    #Cleaning
    df<-df %>% replace_na(list(Level = 'Subsystem'))
    df[is.na(df)] <- "Subsystem"
    
    
    # Select only useful data for analysis
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
  
 
  df<-callModule(choosedf,'bars',dfAbs,dfIO)
  
  # BAR PLOT SCOPES stacked  Interface grouped  ---------------------
  callModule(ChoicesSPL, "Scope", df)
  callModule(ChoicesMultiInterface,'Scope',df)
  callModule(MultibarPlotServerScope,"Scope",df)

  #  BAR CHART BY SYSTEM stacked subsystems  ----
  callModule(ScenarioTimeChoice,'System',df)
  callModule(ChoicesInterface,'System',df)
  callModule(barPlotSubsystemServer,'System',df)



  # BAR CHART BY PROCESSOR AND INTERFACE choice  ------
  callModule(ScenarioTimeChoice, "processor", df)
  callModule(ChoicesScope,"processor",df)
  callModule(ChoicesMultiInterface,'processor',df)
  callModule(ChoicesMultiProcessor,'processor',df)
  callModule(barPlotProcessorInterfaceSertver,'processor',df)


  
  
  # new Reactive EUM ----

  callModule(ChoicesScenario,'EUM',dfAbs)
  callModule(ChoicesPeriod,'EUM',dfAbs)
  callModule(ChoicesSystem,'EUM',dfAbs)
  callModule(ChoicesScope,'EUM',dfAbs)
  callModule(ChoicesFlow,'EUM',dfAbs)
  callModule(ChoicesFund,'EUM',dfAbs)

  # reactive dataframes for indicators ------
  totalEUM<-callModule(matrixEUM,'EUM',dfAbs)
  ShortEUM<-callModule(IndicatorsEUM,'EUM',totalEUM)




  #EUM output en formato excel ----
  output$eum<-renderExcel({
    excelTable(data = ShortEUM())
  })





  #PLOT Indicators ----
  callModule(ChoicesIndicator,'EUMplot',totalEUM)
  callModule(ChoicesLevel,'EUMplot',totalEUM)
  callModule(ChoicesPeriod,'EUMplot',totalEUM)
  callModule(batPlotEUMScope,'EUMplot',totalEUM)





   # Gauge Plot ---
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
    c$append_command(paste0('ScalarIndicators'," ",toString(input$addCommands)),
                     ScalarIndicators())
    c$append_command(paste0('ScalarBenchmarks',' ',toString(input$addCommands)),
                     ScalarBenchmarks())
    })

  # download Commands xlsx
  output$dl <- downloadHandler(
    filename = function() {"Indicators.xlsx"},
    content = function(file) {write_xlsx(list(
      ScalarIndicators = ScalarIndicators(),
      ScalarBenchmarks = ScalarBenchmarks()), path = file)}
    )
  

  # TREE ----
  callModule(ChoicesScope,'tree',dfAbs)
  callModule(ChoicesPeriod,'tree',dfAbs)
  callModule(ChoicesInterface,'tree',dfAbs)
  callModule(TreeServer,'tree',dfAbs)
  callModule(unit,'tree',dfAbs)
  
} #END