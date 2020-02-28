function(input, output) {

  # TAB 1 INPUT FILE TO NIS ----  
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    filename <- inFile$datapath
    # if (length(df$Conflict) != 0){
    #   df<-filter(df, df$Conflict != "Dismissed")
    # }
    
    fname <- filename
    c$login("test_user")
    print("Logged in")
    c$open_session()
    print("Session opened")
    n <- c$load_workbook(fname, "NIS_agent", "NIS_agent@1")
    print(paste("N worksheets: ",n))
    r <- c$submit()
    print("Returned from submit")
    r <- c$query_available_datasets()
    ds <- c$query_datasets(c(tuple("flow_graph_solution", "csv", "dataframe")))
    df <- py_to_r(ds[[1]][[3]])
    df<-subset(df,select = -c(Conflict_Partof,Conflict_Itype,Computed,Expression,Observer))
    df$Value<-as.numeric(lapply(df$Value,str_replace ,pattern = ",",replacement = "."))
    return(df)
  })
  
  
  output$sample_table<- renderDataTable({
    df <- df_products_upload()
    DT::datatable(df)
  })
  
  # TAB 2 FIRST PIE -------
  # INPUTS: 
  
  output$scenario = renderUI({
    datos<-df_products_upload()
    Scenarios <<- as.vector(unique(datos$Scenario))
    selectInput('scenario', "Choose a Scenario:", Scenarios)
  })
  
  output$scope = renderUI({
    datos<-df_products_upload()
    Scopes<<- as.vector(unique(datos$Scope))
    selectInput('scope', "Choose a Scope:", Scopes)
  })
  
  
  output$period = renderUI({
    datos<-df_products_upload()
    datos$Period<-as.numeric(datos$Period)
    Periods<<- as.vector(unique(datos$Period))
    selectInput('period',  "Choose a Period:", Periods, selected = 2017)
  })
  
  
  output$level = renderUI({
    datos<-df_products_upload()
    Level <<-as.vector(unique(datos$Level))
    selectInput('level', "Choose a level:", Level)
  })
  
  
  
  output$interface = renderUI({
    datos<-df_products_upload()
    Interfaces<<- as.vector(unique(datos$Interface))
    selectInput('interface', "Choose an Interface:", Interfaces)
  })
  
  
  
  
  #PieChart Level
  
  output$PiePlot <- renderPlot({
    if (input$act==0)
      return()
    
    #TODO  % Value
    
    datos<<-df_products_upload()
    dff <<- filter(datos,datos$Scenario == input$scenario & datos$Period == input$period & datos$Level == input$level & datos$Interface == input$interface, datos$Scope == input$scope)
    dff$per<-round(dff$Value/sum(dff$Value)*100, digits = 3)
    dff$names_per <-paste(dff$Processor,dff$per,"%", sep = " ")
    plt <- ggplot (dff, aes( x = "" ,  y = Value, fill = names_per)) + geom_bar(width = 1, stat = "identity")
    pie <- plt + coord_polar("y", start=0)
    pie
  })
  
  
  
  
  # TAB 3 PIE BY SYSTEM  ----
  # Reactive Inputs
  
  output$scenario2 = renderUI({
    datos<-df_products_upload()
    Scenarios <- as.vector(unique(datos$Scenario))
    selectInput('scenario2', "Choose a Scenario:", Scenarios)
  })
  
  output$scope2 = renderUI({
    datos<-df_products_upload()
    Scopes<<- as.vector(unique(datos$Scope))
    selectInput('scope2', "Choose a Scope:", Scopes)
  })
  
  
  output$period2 = renderUI({
    datos<-df_products_upload()
    datos$Period<-as.numeric(datos$Period)
    Periods<- as.vector(unique(datos$Period))
    selectInput('period2',  "Choose a Period:", Periods)
  })
  
  
  
  output$interface2 = renderUI({
    datos<-df_products_upload()
    Interfaces<- as.vector(unique(datos$Interface))
    selectInput('interface2', "Choose an Interface:", Interfaces)
  })
  
  
  
  #PieChart System
  output$PiePlotSystem <- renderPlot({
    if (input$act==0)
      return()
    #TODO  % Values 
    data<-df_products_upload()
    
    
    #df$valper <- round(df$Value/sum(df$Value)*100, digits = 3)
    plt <- ggplot (df, aes( x = "" ,  y = Value, fill = System)) + geom_bar(width = 1, stat = "identity")
    pie <- plt + coord_polar("y", start=0)
    pie
  })
  
  
  
  #TAB 4 PIE BY PROCESSOR ------
  # Reactive Inputs:
  
  output$scenario3 = renderUI({
    datos<-df_products_upload()
    Scenarios <- as.vector(unique(datos$Scenario))
    selectInput('scenario3', "Choose a Scenario:", Scenarios)
  })
  
  output$scope3 = renderUI({
    datos<-df_products_upload()
    Scopes<- as.vector(unique(datos$Scope))
    selectInput('scope3', "Choose a Scope:", Scopes)
  })
  
  output$period3 = renderUI({
    datos<-df_products_upload()
    datos$Period<-as.numeric(datos$Period)
    Periods<- as.vector(unique(datos$Period))
    selectInput('period3',  "Choose a Period:", Periods)
  })
  
  
  
  output$interface3 = renderUI({
    datos<-df_products_upload()
    Interfaces<- as.vector(unique(datos$Interface))
    selectInput('interface3', "Choose an Interface:", Interfaces)
  })
  
  output$ProcessorsChoice = renderUI({
    datos<-df_products_upload()
    Processors<- as.vector(unique(datos$Processor))
    checkboxGroupInput("ProcessorsChoice", "Processors to compare:",
                       choiceNames = Processors, choiceValues = Processors, selected = Processors[1])
  })
  
  #PieChart Processors
  output$PiePlotProcessors <- renderPlot({
    if (input$act==0)
      return()
    #TODO  % Values
    data<-df_products_upload()  
    df <- filter(data,data$Scenario == input$scenario3 &  data$Period == input$period3  &  data$Interface == input$interface3, data$Scope == input$scope3)
    df <- filter(df, Processor %in% input$ProcessorsChoice, )
    df$per<-round(df$Value/sum(df$Value)*100, digits = 3)
    df$names_per <-paste(df$Processor,df$per,"%", sep = " ")
    plt <- ggplot (df, aes( x = "" ,  y = Value, fill = names_per)) + geom_bar(width = 1, stat = "identity")
    pie <- plt + coord_polar("y", start=0)
    pie
    
  })
  
  
  
  
  # #BarChart
  #
  # output$BarPlot <- renderPlot({
  #
  #   df<-filter(LE,LE$Scenario == input$scenario & LE$Period == input$period & LE$Processor == input$processor)
  #   #remouving ".Input" ".Output" from interfaces
  #   xx = c(names(df[-c(1,2,3,4)]))
  #   x<-str_remove(xx,'.Output')
  #   x<-str_remove(x,'.Input')
  #   l<-x
  #   names(l)<-x
  #
  #
  #   if (input$log_10){
  #     y = log10(abs(as.numeric(df[1,-c(1,2,3,4)])))*(abs(as.numeric(df[1,-c(1,2,3,4)]))/as.numeric(df[1,-c(1,2,3,4)]))
  #   }
  #   else{
  #     y = as.numeric(df[1,-c(1,2,3,4)])
  #   }
  #
  #   bar <-data.frame(x,y)
  #   #ba   r<-filter(bar,bar$x %in% c(input$checkGroup))
  #
  #
  #   # Fill in the spot we created for a plot
  #
  #   p = ggplot(bar, aes(x,y)) +
  #     geom_bar(stat = "identity", aes(fill = x), legend = FALSE)
  #   p+theme(axis.text.x = element_text(size = 10, angle = 90))
  # })
  
  eum<-reactive({
    data<-df_products_upload()
    df<-filter(data,data$Scope == input$ScopeChoice, data$Scenario == input$ScenarioChoice , data$Period == input$PeriodChoice, data$System == input$SystemChoice)
    # TODO needed?:
    # if (length(df$Conflict) != 0){
    #   df<-filter(df, df$Conflict != "Dismissed")
    # }
    eumflow <- filter(df, Interface %in% input$show_Interfaces, )
    eumfund <- filter(df, df$Interface == input$FundInterface)
    cat(class(eumflow))
    
    eum <- merge(x = eumflow,y = eumfund, by = "Processor")
    eum$Valueeum <- eum$Value.x/eum$Value.y
    eum$Valuepop<-eum$Value.x/input$Population
    
    eum$InterfaceUnit<-paste(paste(eum$Interface.x,eum$Interface.y,sep = "/"),paste(eum$Unit.x,eum$Unit.y,sep = "/"),sep=" ")
    eumInterface <- eum%>%select(Level.x,Processor, Valueeum,InterfaceUnit)%>% spread(InterfaceUnit,Valueeum)
    eumInterface<-`colnames<-`(eumInterface,c("Level", colnames(eumInterface[-1])))
    
    eumpop<-eum%>%select(Processor, Valuepop, Interface.x)%>% spread(Interface.x,Valuepop)
    colnamesEumpop<-paste(colnames(eumpop)[-1], "cap", sep = "/")
    eumpop<-`colnames<-`(eumpop,c("Processor",colnamesEumpop))
    
    eum <- merge(eumpop,eumInterface, by = "Processor")
    
    #Merge fund column with unit
    eumfund$Interface_Unit<-paste(eumfund$Interface, eumfund$Unit, sep = " ")
    eum<-merge(eumfund%>%select(Processor,Interface_Unit,Value)%>%unique(),eum, by = "Processor")
    
    
    eum<-eum[order(eum$Level),]
    
  })
  
  
  # TAB 5 EUM 1 -----
  #Reactive Inputs
  
  output$FundInterface= renderUI({
    data<- df_products_upload()
    Fund<- filter(data, data$RoegenType == "Fund" | data$RoegenType == "fund")
    FundInterfaces <- as.vector(unique(Fund$Interface))
    selectInput("FundInterface", "Choose a Fund InterfaceType:",
                choices = FundInterfaces,selected = FundInterfaces[1])
  })
  
  output$ScopeChoice = renderUI({
    data<-df_products_upload()
    Scopes<- as.vector(unique(data$Scope))
    selectInput("ScopeChoice", "Choose a Scope:",
                choices = Scopes, selected = Scopes[1]) 
  })
  
  
  output$ScenarioChoice = renderUI({
    data<-df_products_upload()
    Scenarios <- as.vector(unique(data$Scenario))
    selectInput("ScenarioChoice", "Choose a Scenario:",
                choices = Scenarios)
  })
  output$PeriodChoice = renderUI({
    data<-df_products_upload()
    data$Period<-as.numeric(data$Period)
    Periods<- as.vector(unique(data$Period))
    selectInput("PeriodChoice", "Choose a Period:",
                choices = Periods, selected = Periods[length(Periods)])
    
  })
  output$SystemChoice = renderUI({
    data<-df_products_upload()
    Systems<- as.vector (unique(data$System))
    selectInput("SystemChoice", "Choose a System:",
                choices = Systems, selected = Systems[1])
  })
  
  output$show_Interfaces = renderUI({
    data<- df_products_upload()
    Flow<- filter(data, data$RoegenType == "flow" | data$RoegenType == "Flow")
    FlowInterfaces<- as.vector(unique(Flow$Interface))
    checkboxGroupInput("show_Interfaces", "Choose a flow InterfaceType to show:",
                       choiceNames = FlowInterfaces, choiceValues = FlowInterfaces, selected = FlowInterfaces[1])
  })
  
  
  
  # tabla EUM SIN FORMATO EXCEL (NO EN USO) --------
  # output$eum<- DT::renderDataTable({
  #   if (input$act==0)
  #     return()
  
  #renderTable({
  # eum()
  #    })
  
  # })
  
  
  
  #TAB 6 EUM FORMATO EXCEL ----
  
  output$eum<-  renderExcel({
    if (input$act==0)
      return()
    excelTable(data = eum())
  })
  
  
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
  
  
  # TAB 8 INDICATORS ----
  #Reactive input
  output$indicator = renderUI({
    eum <- eum()
    ind <- colnames(eum[setdiff(names(eum), c("Processor","Interface_Unit","Value", "Level"))])
    selectInput("indicator", "Choose a indicator:",
                choices = ind)
    
  })
  output$LevelIndicator = renderUI({
    eum<-eum()
    Levels<-as.vector(unique(eum$Level))
    selectInput("LevelIndicator", "Choose a level to analize:",
                choices = Levels)
    
  })
  
  
  #plot
  
  output$gaugePlot <- renderPlot({
    if (input$act==0)
      return()
    eum <- eum()
    eumlevel<-filter(eum,eum$Level == input$LevelIndicator)
    eumindicator<- eumlevel[c("Processor", input$indicator)]
    gg.gauge(eumindicator,breaks = c(0,input$break2,input$break3,100))
    #TODO problem displaying text.. bad visualization of text...
  }, height = 400, width = 800 ) #this one seems to not change anything
  
  # TAB 7 TREE WITH QUANTITIES -----
  #outputs
  
  output$ScopeTree = renderUI({
    data<- df_products_upload()
    Scopes<- as.vector(unique(data$Scope))
    selectInput("ScopeTree", "Choose a Scope:",
                choices = Scopes)
  })
  
  output$PeriodTree = renderUI({
    data<-df_products_upload()
    data$Period<-as.numeric(data$Period)
    Periods<- as.vector(unique(data$Period))
    selectInput("PeriodTree", "Choose a Period:",
                choices = Periods)
  })
  
  
  #tree Output
  output$Tree<-renderCollapsibleTree({
    if (input$act==0)
      return()
    #    isolate({
    data<-df_products_upload()
    Level <-as.vector(unique(data$Level))
    datafilter<-filter(data,data$Scope == input$ScopeTree, data$Period == input$PeriodTree)
    tree<-datafilter%>%separate(Processor,c(Level), sep= "\\.")
    collapsibleTree(df = tree, c(Level), fill = "green", width = 800)
    
    #    })
  })
  
  
  
  # TAB TREE 2 ----
  # REACTIVE INPUTS
  output$ScopeTree2 = renderUI({
    data<- df_products_upload()
    Scopes<- as.vector(unique(data$Scope))
    selectInput("ScopeTree2", "Choose a Scope:",
                choices = Scopes)
    
  })
  
  output$PeriodTree2 = renderUI({
    data<-df_products_upload()
    data$Period<-as.numeric(data$Period)
    Periods<- as.vector(unique(data$Period))
    selectInput("PeriodTree2", "Choose a Period:",
                choices = Periods)
    
  })
  
  output$InterfaceTree2 = renderUI({
    data<-df_products_upload()
    Interfaces<- as.vector(unique(data$Interface))
    selectInput("InterfaceTree2", "Choose an Interface:",
                choices = Interfaces)
    
  })
  
  
  output$TreeInterface<-renderCollapsibleTree({
    if (input$act==0)
      return()
    #    isolate({
    data<-df_products_upload()
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
    data<- df_products_upload()
    Unit<- paste("Unit",filter(data,data$Interface == input$InterfaceTree2)$Unit[1],sep = "=")
    
  })  
  
  
} #END