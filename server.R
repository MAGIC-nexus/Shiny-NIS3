function(input, output,session) {
  
  #close session and windows
 
  #log out when close the windows
  session$onSessionEnded(function() {
    print(c$close_session())
    print(c$logout())
    stopApp()
  })
  
  
  
  # INPUT FILE TO NIS ----  
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    filename <- inFile$datapath
    c <- nexinfosys$NISClient("https://one.nis.magic-nexus.eu/nis_api")
    fname <- filename
    try({print(c$close_session())
         print(c$logout())
    }, silent = FALSE)
    
    print(c$login("test_user"))
    # output$logged<-renderText({"Logged in"})
    c$open_session()
    # output$opened<-renderText({"Session opened"})
    n <- c$load_workbook(fname, "NIS_agent", "NIS_agent@1")
    output$worksheets<-renderText({paste("N worksheets: ",n)})
    r <- c$submit()
    num_errors<- 0
    if (length(r) > 0){
      issues <- data.frame(matrix(unlist(r), nrow=length(r), byrow=T, ncol = 5))
      colnames(issues)<-c("sheet","message","row","sheet_name","type")
      for(i in r){
        if (i["type"] == 3){
          num_errors <-num_errors + 1
        }
      }
      output$num_errors<-renderText({toString(num_errors)})
    }
    print("Returned from submit")
    if (num_errors == 0){
      r <- c$query_available_datasets()
      ds <- c$query_datasets(c(tuple("flow_graph_solution", "csv", "dataframe")))
      df <- py_to_r(ds[[1]][[3]])
      #TODO quitar esto y poner todos los resultados
      #TODO tendré que cambiar más código 
      df<-subset(df,select = -c(Conflict_Partof,Conflict_Itype,Computed,Expression,Observer))
      df$Value<-as.numeric(lapply(df$Value,str_replace ,pattern = ",",replacement = "."))
    }else{
      df<-issues
    }
      list(df1 = df, df2 = issues)

  })
  
  output$sample_table<- renderDataTable({
    df <- df_products_upload()[['df1']]
    DT::datatable(df)
  })
  
  output$issues<- renderDataTable({
    df <- df_products_upload()[['df2']]
    DT::datatable(df)
  })
  
  
 
  # BAR CHART PLOT BY LEVEL -------
  # INPUTS: 
  
  output$scenario = renderUI({
    datos<-df_products_upload()[['df1']]
    Scenarios <- as.vector(unique(datos$Scenario))
    selectInput('scenario', "Choose a Scenario:", Scenarios)
  })
  
  output$scope = renderUI({
    datos<-df_products_upload()[['df1']]
    Scopes<- as.vector(unique(datos$Scope))
    selectInput('scope', "Choose a Scope:", Scopes)
  })
  
  
  output$period = renderUI({
    datos<-df_products_upload()[['df1']]
    datos$Period<-as.numeric(datos$Period)
    Periods<- as.vector(unique(datos$Period))
    selectInput('period',  "Choose a Period:", Periods, selected = 2017)
  })
  
  
  output$level = renderUI({
    datos<-df_products_upload()[['df1']]
    Level <-as.vector(unique(datos$Level))
    selectInput('level', "Choose a level:", Level)
  })
  
  
  output$InterfacesChoice1 = renderUI({
    datos<-df_products_upload()[['df1']]
    Interfaces<- as.vector(unique(datos$Interface))
    checkboxGroupInput("InterfacesChoice1", "InterfacesTypes:",
                       choiceNames = Interfaces, choiceValues = Interfaces, selected = Interfaces[1])
  })
  
  
  
  
  
  # Plot by Level-----
  
  output$PiePlot <- renderPlot({
    if (input$act==0)
      return()
    
    #TODO  % Value
    # TODO nombre de los processors en vertical
    data<-df_products_upload()[['df1']]
    df <- filter(data,data$Scenario == input$scenario & data$Period == input$period & data$Level == input$level, data$Scope == input$scope)
    df <- filter(df, Interface %in% input$InterfacesChoice1, )
    df$per<-round(df$Value/sum(df$Value)*100, digits = 3)
    df$names_per <-paste(df$Processor,df$per,"%", sep = " ")
    UnitList<- unique(df$Unit)
    validate(
      need(length(UnitList)==1, "Your interface selection should have the same unit") 
    )
    barchart <- ggplot (df, aes( x = Processor ,  y = Value, fill = Interface)) + geom_bar( position="dodge", stat = "identity") + 
      labs(title = "Inrterface value", y = unique(df$Unit)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    barchart
  })
  
  
  
  #  BAR CHART BY SYSTEM  ----
  # Reactive Inputs
  
  output$scenario2 = renderUI({
    datos<-df_products_upload()[['df1']]
    Scenarios <- as.vector(unique(datos$Scenario))
    selectInput('scenario2', "Choose a Scenario:", Scenarios)
  })
  
  output$scope2 = renderUI({
    datos<-df_products_upload()[['df1']]
    Scopes<<- as.vector(unique(datos$Scope))
    selectInput('scope2', "Choose a Scope:", Scopes)
  })
  
  
  output$period2 = renderUI({
    datos<-df_products_upload()[['df1']]
    datos$Period<-as.numeric(datos$Period)
    Periods<- as.vector(unique(datos$Period))
    selectInput('period2',  "Choose a Period:", Periods)
  })
  
  
  
  output$InterfacesChoice2 = renderUI({
    datos<-df_products_upload()[['df1']]
    Interfaces<- as.vector(unique(datos$Interface))
    checkboxGroupInput("InterfacesChoice2", "InterfacesTypes:",
                       choiceNames = Interfaces, choiceValues = Interfaces, selected = Interfaces[1])
  })
  
  
  
  #plot by System----
  output$PiePlotSystem <- renderPlot({
    if (input$act==0)
      return()
    #TODO  % Values 
    data<-df_products_upload()[['df1']]
    df <- filter(data,data$Scenario == input$scenario2 & data$Period == input$period2 , data$Scope == input$scope2)
    df <- filter(df, Interface %in% input$InterfacesChoice2, )
    UnitList<- unique(df$Unit)
    validate(
      need(length(UnitList)==1, "Your interface selection should have the same unit") 
    )
    barchart <- ggplot (df, aes( x = System ,  y = Value, fill = Interface)) + geom_bar( position="dodge", stat = "identity") + 
      labs(title = "Inrterface value", y = unique(df$Unit)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    barchart
  })
  
  
  
  # BAR CHART BY PROCESSOR ------
  # Reactive Inputs:
  
  output$scenario3 = renderUI({
    datos<-df_products_upload()[['df1']]
    Scenarios <- as.vector(unique(datos$Scenario))
    selectInput('scenario3', "Choose a Scenario:", Scenarios)
  })
  
  output$scope3 = renderUI({
    datos<-df_products_upload()[['df1']]
    Scopes<- as.vector(unique(datos$Scope))
    selectInput('scope3', "Choose a Scope:", Scopes)
  })
  
  output$period3 = renderUI({
    datos<-df_products_upload()[['df1']]
    datos$Period<-as.numeric(datos$Period)
    Periods<- as.vector(unique(datos$Period))
    selectInput('period3',  "Choose a Period:", Periods)
  })
  
  
  output$ProcessorsChoice = renderUI({
    datos<-df_products_upload()[['df1']]
    Processors<- as.vector(unique(datos$Processor))
    checkboxGroupInput("ProcessorsChoice", "Processors to compare:",
                       choiceNames = Processors, choiceValues = Processors, selected = Processors[1])
  })
  
  output$InterfacesChoice3 = renderUI({
    datos<-df_products_upload()[['df1']]
    Interfaces<- as.vector(unique(datos$Interface))
    checkboxGroupInput("InterfacesChoice3", "InterfacesTypes:",
                       choiceNames = Interfaces, choiceValues = Interfaces, selected = Interfaces[1])
  })
  
  #Plot by Processors------
  output$PiePlotProcessors <- renderPlot({
    if (input$act==0)
      return()
    data<-df_products_upload()[['df1']] 
    df <- filter(data,data$Scenario == input$scenario3 &  data$Period == input$period3, data$Scope == input$scope3)
    df <- filter(df, Processor %in% input$ProcessorsChoice, )
    df <- filter(df, Interface %in% input$InterfacesChoice3, )
    df$per<-round(df$Value/sum(df$Value)*100, digits = 3)
    df$names_per <-paste(df$Processor,df$per,"%", sep = " ")
    # plt <- ggplot (df, aes( x = "" ,  y = Value, fill = names_per)) + geom_bar(width = 1, stat = "identity")
    # pie <- plt + coord_polar("y", start=0)
    # pie
    # TODO try to represents levels aswell better only allow to reppresent interfaces wilth same units
    UnitList<- unique(df$Unit)
    validate(
      need(length(UnitList)==1, "Your interface selection should have the same unit") 
    )
    
    
    barchart <- ggplot (df, aes( x = Processor ,  y = Value, fill = Interface)) + geom_bar( position="dodge", stat = "identity") + 
      labs(title = "Inrterface value", y = unique(df$Unit)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    barchart
    
  })
  


  # create balance flow graph solution-----
  
  BalanceEUM<-reactive({
    dat<<-df_products_upload()[['df1']]
    data_spread<-data%>%spread(Orientation,value = Value)
    data_spread$balance <- (flow_balanace$Input - flow_balanace$Output)
    data_gather<-data_gather%>%gather(key = c("Orientation", "Value"), value = c("Input","Output","balance"))
  })
  
  
  # new Reactive EUM (all indicators in the same column) ----
  
  totalEUM<-reactive({
    data<-df_products_upload()[['df1']]
    # data <- BalanceEUM()
    df<-filter(data, data$Scenario == input$ScenarioChoice)
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
                         'Orientation' = as.character(),
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
            'Orientation' = eum$Orientation,
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
          'Orientation' = eum$Orientation,
          stringsAsFactors = FALSE
        )
        eumTotal<-rbind(eumTotal,eumTmp,stringsAsFactors = FALSE)
      } 
      
    }

    eum<-eumTotal[!duplicated(eumTotal), ]
    

    
  })
  
  #eum formato corto ----
    
  ShortEUM<-reactive({
    eum<-totalEUM()
    eum<-filter(eum,System == input$SystemChoice, Scope == input$ScopeChoice  , Period == input$PeriodChoice)
    eum<-eum[,!(colnames(eum) %in% c("indicator","Flow","Fund","Unit","System"))]%>%spread(indicator_Unit,value = Value)
  }) 
  
  
  
  # EUM -----
  #Reactive Inputs
  
  output$FundInterface= renderUI({
    data<- df_products_upload()[['df1']]
    Fund<- filter(data, data$RoegenType == "Fund" | data$RoegenType == "fund")
    FundInterfaces <- as.vector(unique(Fund$Interface))
    checkboxGroupInput("FundInterface", "Choose a Fund InterfaceType:",
                choices = FundInterfaces,selected = FundInterfaces[1])
  })
  
  output$ScopeChoice = renderUI({
    data<-df_products_upload()[['df1']]
    Scopes<- as.vector(unique(data$Scope))
    selectInput("ScopeChoice", "Choose a Scope:",
                choices = Scopes, selected = Scopes[1]) 
  })
  
  
  output$ScenarioChoice = renderUI({
    data<-df_products_upload()[['df1']]
    Scenarios <- as.vector(unique(data$Scenario))
    selectInput("ScenarioChoice", "Choose a Scenario:",
                choices = Scenarios)
  })
  output$PeriodChoice = renderUI({
    data<-df_products_upload()[['df1']]
    data$Period<-as.numeric(data$Period)
    Periods<- as.vector(unique(data$Period))
    selectInput("PeriodChoice", "Choose a Period:",
                choices = Periods, selected = Periods[length(Periods)])
    
  })
  output$SystemChoice = renderUI({
    data<-df_products_upload()[['df1']]
    Systems<- as.vector (unique(data$System))
    selectInput("SystemChoice", "Choose a System:",
                choices = Systems, selected = Systems[1])
  })
  
  output$show_Interfaces = renderUI({
    data<- df_products_upload()[['df1']]
    Flow<- filter(data, data$RoegenType == "flow" | data$RoegenType == "Flow")
    FlowInterfaces<- as.vector(unique(Flow$Interface))
    checkboxGroupInput("show_Interfaces", "Choose a flow InterfaceType to show:",
                       choiceNames = FlowInterfaces, choiceValues = FlowInterfaces, selected = FlowInterfaces[1])
  })
  
  
  # tabla EUM SIN FORMATO EXCEL (NO EN USO)
  # output$eum<- DT::renderDataTable({
  #   if (input$act==0)
  #     return()
  
  #renderTable({
  # eum()
  #    })
  
  # })
  
  
  
  #EUM output en formato excel ----
  
  output$eum<-  renderExcel({
    if (input$act==0)
      return()
    excelTable(data = ShortEUM())
  })
  
    # rpivotTable(data = eum()   ,  rows = "Processor",cols="Level",
    # vals = "Value", aggregatorName = "Sum", rendererName = "Table",
    # width="100%", height="500px")
  # })
  
  
  #BOXPLOTS NO EN USO ---- 
  
  # output$boxplot <- renderPlot({
  #   # #renderTable({
  #   # df<-filter(data,data$Scope == input$ScopeChoice, data$Scenario == input$ScenarioChoice , data$Period == input$PeriodChoice, data$System == input$SystemChoice)
  #   # if (length(df$Conflict) != 0){
  #   #   df<-filter(df, df$Conflict != "Dismissed")
  #   # }
  #   # eumflow <- filter(df, Interface %in% input$show_Interfaces, )
  #   # eumfund <- filter(df, df$Interftace == input$FundInterface)
  #   #
  #   # eum <- merge(x = eumflow,y = eumfund, by = "Processor")
  #   # eum$Valueeum <- eum$Value.x/eum$Value.y
  #   # eum$Valuepop<-eum$Value.x/input$Population
  #   #
  #   # eum$InterfaceUnit<-paste(paste(eum$Interface.x,eum$Interface.y,sep = "/"),paste(eum$Unit.x,eum$Unit.y,sep = "/"),sep=" ")
  #   # eumInterface <- eum%>%select(Level.x,Processor, Valueeum,InterfaceUnit)%>% spread(InterfaceUnit,Valueeum)
  #   # eumInterface<-`colnames<-`(eumInterface,c("Level", colnames(eumInterface[-1])))
  #   #
  #   #
  #   #
  #   # eumpop<-eum%>%select(Processor, Valuepop, Interface.x)%>% spread(Interface.x,Valuepop)
  #   # colnamesEumpop<-paste(colnames(eumpop)[-1], "cap", sep = "/")
  #   # eumpop<-`colnames<-`(eumpop,c("Processor",colnamesEumpop))
  #   #
  #   #
  #   # eum <- merge(eumpop,eumInterface, by = "Processor")
  #   #
  #   # #Merge fund column with unit
  #   # eumfund$Interface_Unit<-paste(eumfund$Interface, eumfund$Unit, sep = " ")
  #   # eum<-merge(eumfund%>%select(Processor,Interface_Unit,Value)%>%unique(),eum, by = "Processor")
  #
  #
  #   Indicators<<- a
  #   output<-paste(Indicatorshow,b,sep = " ")
  #   boxplot <- filter(eum, Processor %in% input$ProcessorsChoice, )
  #   bp<- ggplot(boxplot, aes(x = Processor, y = input$Indicatorshow)) + geom_boxplot()
  #   bp
  #
  # })
  
  
  #PLOT EXTERAL VS INTERNAL ----
  
  #inputs
  output$indicator2 = renderUI({
    df <- totalEUM()
    ind <- unique(df$indicator)
    selectInput("indicator2", "Choose a indicator:",
                choices = ind)
    
  })
  
  output$LevelIndicator2 = renderUI({
    df<-totalEUM()
    Levels<-as.vector(unique(df$Level))
    selectInput("LevelIndicator2", "Choose a level to analize:",
                choices = Levels)
  })
  
  # output$SubSystemChoice2 = renderUI({
  #   df<-totalEUM()
  #   Susbsystems<-as.vector(unique(df$Subsystem))
  #   checkboxGroupInput("SubSystemChoice2", "Choose Subsystems to compare:",
  #               choices = Susbsystems, selected = Susbsystems[1])
  # })
  

  output$ExternalInternalPlot<-renderPlot({
    df<- totalEUM()
    dfplot<-filter(df, indicator == input$indicator2, Level == input$LevelIndicator2) #tabla con 1 indicador varios sistemas agrupados por externo e interno 
    dfplot <- filter(dfplot, Scope %in% c("Internal", "External"), )
    ggplot(dfplot, aes(y = Value, x = Processor, fill = Scope)) + geom_bar(Position = 'stack' , stat = 'identity') + 
      labs(title = "Inrterface value", y = unique(dfplot$Unit[1])) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

  })

  
  
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
    eum<-filter(eum, Scope == input$ScopeChoice, Level == input$LevelIndicator2)
    # eum$ID <-seq.int(nrow(eum)) #TODO esto es una chanada, las filas no se deberían repetar
    eum<-eum[,!(colnames(eum) %in% c("Flow","Fund","Unit","indicator"))]%>%spread(indicator_Unit,value = Value)
    ggplot(eum, aes(eum$input$ind1 , eum$input$ind2)) +
      geom_point(aes(colour =  factor( System), size = eum[input$ind3], alpha = Period))
  })
  
  #inputs
  output$indicator = renderUI({
    df <- totalEUM()
    ind <- unique(df$indicator)
    selectInput("indicator", "Choose a indicator:",
                choices = ind)
    
  })
  
  output$LevelIndicator = renderUI({
    df<-totalEUM()
    Levels<-as.vector(unique(df$Level))
    selectInput("LevelIndicator", "Choose a level to analize:",
                choices = Levels)
  })
  
  output$ScopeIndicator = renderUI({
    df<-totalEUM()
    Levels<-as.vector(unique(df$Scope))
    selectInput("ScopeIndicator", "Choose a level to analize:",
                choices = Levels)
  })
  
  
  #plot gauge----
  
  output$gaugePlot <- renderPlot({
    if (input$act==0)
      return()
    eum <- totalEUM()
    eumindicator<-filter(eum,eum$Level == input$LevelIndicator,indicator == input$indicator, eum$Scope == input$ScopeIndicator)
    # eumindicator<- eumlevel[c("Processor", input$indicator)]
    gg.gauge(eumindicator,breaks = c(input$break1,input$break2,input$break3,input$break4), colour = c(input$colour1,input$colour2,input$colour3))
    #TODO problem displaying text.. bad visualization of text...
  }, height = 400, width = 800 ) #this one seems to not change anything
  
  #Create command -----
  
  # Scalarbenchmark<- data.frame(
  #  'benchMarkGroup' = as.character(),
  #  'Stakeholders' = as.character(),
  #  'Range' = as.character(),
  #  'Category' = as.character(),
  #  'Label' = as.character()
  # )

  
  
  
  # TREE WITH QUANTITIES -----
  #outputs
  
  output$ScopeTree = renderUI({
    data<- df_products_upload()[['df1']]
    Scopes<- as.vector(unique(data$Scope))
    selectInput("ScopeTree", "Choose a Scope:",
                choices = Scopes)
  })
  
  output$PeriodTree = renderUI({
    data<-df_products_upload()[['df1']]
    data$Period<-as.numeric(data$Period)
    Periods<- as.vector(unique(data$Period))
    selectInput("PeriodTree", "Choose a Period:",
                choices = Periods)
  })
  
  
  # # EDITABLE EUM ----
  # output$editableTable<-renderRHandsontable({
  #   df<-ShortEUM()
  #   rhandsontable(df)
  #   
  #   
  # })
  
  observeEvent(input$saveData,
               write.csv(hot_to_r(input$editableTable, file = MyData.csv, row.names = TRUE)))
  
  #TODO la idea aquí es que al editar un indicador, como los fund se mantienen iguales, se edite una interface (o edito funds o edito flows, o mantengo la relación (lo que tenga sentido)
  #editada la interface, puedo ver cómo cambian las el procesor al que me estoy refiriendo (solo ese prossor) lo que se traduciría en un cambio en todos los demás indicadores
  # VER SI ESTO LO PUEDO HACER CON LAS EXPRESIONES
  
  
  #tree Output ----
  output$Tree<-renderCollapsibleTree({
    if (input$act==0)
      return()
    #    isolate({
    data<-df_products_upload()[['df1']]
    Level <-as.vector(unique(data$Level))
    datafilter<-filter(data,data$Scope == input$ScopeTree, data$Period == input$PeriodTree)
    tree<-datafilter%>%separate(Processor,c(Level), sep= "\\.")
    collapsibleTree(df = tree, c(Level), fill = "green", width = 800)
    
    #    })
  })
  
  
  
  # TREE ----
  # REACTIVE INPUTS
  output$ScopeTree2 = renderUI({
    data<- df_products_upload()[['df1']]
    Scopes<- as.vector(unique(data$Scope))
    selectInput("ScopeTree2", "Choose a Scope:",
                choices = Scopes)
    
  })
  
  output$PeriodTree2 = renderUI({
    data<-df_products_upload()[['df1']]
    data$Period<-as.numeric(data$Period)
    Periods<- as.vector(unique(data$Period))
    selectInput("PeriodTree2", "Choose a Period:",
                choices = Periods)
    
  })
  
  output$InterfaceTree2 = renderUI({
    data<-df_products_upload()[['df1']]
    Interfaces<- as.vector(unique(data$Interface))
    selectInput("InterfaceTree2", "Choose an Interface:",
                choices = Interfaces)
    
  })
  
  #tree ouput ----
  # todo NO FUNCIONA
  output$TreeInterface<-renderCollapsibleTree({
    if (input$act==0)
      return()
    #    isolate({
    data<-df_products_upload()[['df1']]
    Level <-as.vector(unique(data$Level))
    datafilter<-filter(data,data$Scope == input$ScopeTree2,data$Period == input$PeriodTree2, data$Interface == input$InterfaceTree2)
    tree<-datafilter%>%separate(Processor,c(Level), sep= "\\.")
    collapsibleTree(df = tree, c(Level),
                    fill = "green",
                    width = 800,
                    # TODO controlar que el componente agregue desde el último nivel o no. En este momento está agregando aunque se le da todos los valores.
                    # YA HE INTENTADO PONER aggfun = null PERO PARECE QUE SOLO ADMITE MEAN O SUM,,,, ESTO ES UN  PROBLEMA SOLO EN EL CASO DE CONFLICTO DE DATOS
                    # YA QUE EN ESTE CASO SOLO SE LE DA EL RESULTADO AGREGADO BOTTON UP....
                    zoomable = FALSE,
                    tooltip = TRUE,
                    attribute = "Value",
                    nodeSize = "Value")
    
    #   })
    
    
  })
  output$Unit <- renderText({
    if (input$act==0)
      return()
    data<- df_products_upload()[['df1']]
    Unit<- paste("Unit",filter(data,data$Interface == input$InterfaceTree2)$Unit[1],sep = "=")
    
  })  
  
} #END