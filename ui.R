# Define UI for application that draws a histogram
#ui<-# table view of the generated dist ribution
shinyUI(navbarPage("MuSIASEM data visualizations", id = "nav", inverse = TRUE,
                   
                   # New tab with file input and table display -----
                   tabPanel("INPUT FILE ",
                            sidebarLayout(
                              sidebarPanel(
                                fileInput('target_upload', 'Choose file to upload',
                                          accept =".xlsx"
                                          ),
                                actionButton("act","click to update dataset input"),
                                # actionButton("close","CLOSE NIS SESSION BEFORE LEAVE"), #ENCONTRAR ALGO QUE LO HAGA AUTOMÁTICAMENTE PARA CERRAR LA VENTANA
                                # br(),
                                # p("Choose your flow_graph_solution.csv and your separator in your csv file (usally ',') 
                                # When the table is displayed properly click the botton ' 
                                # click to update dataset input in order to navigate through the others tabs")
                              ),
                              mainPanel(
                                DT::dataTableOutput("sample_table")
                              )
                            )
                   )
                   #end of Tab     

                   # New tab with pie chart Levels-Interfaces ----
                   ,tabPanel("Bar chart by level",
                             sidebarLayout(
                               sidebarPanel(
                                 #selectInput("scenario", "Choose a Scenario:",
                                 #            choices = Scenarios),
                                 uiOutput('scenario'),
                                 #selectInput("scope", "Choose a Scope:",
                                 #            choices = Scopes),
                                 uiOutput('scope'),
                                 #selectInput("period", "Choose a Period:",
                                 #            choices = Periods),
                                 uiOutput('period'),
                                 #selectInput("level", "Choose a level:",
                                 #            choices = Level),
                                 uiOutput('level'),
                                 #selectInput("interface", "Choose an Interface:",
                                 #            choices = Interfaces),
                                 uiOutput("InterfacesChoice1"),
                                 br(),
                                 p("This bar plot shows the contribution of the selected interfaces ( eg: water, LU..etc) to each processor of the specified level. Scenario Scope and Period needs to be chosen aswell ")
                                 
                               ),
                               mainPanel(
                                 plotOutput("PiePlot")
                               )
                             )
                   )
                   
                   #end of Tab
                   
                   # New tab with pie chart Systems-Interfaces -----
                   ,tabPanel("Bar chart by System",
                             sidebarLayout(
                               sidebarPanel(
                                 # selectInput("scenario2", "Choose a Scenario:",
                                 #             choices = Scenarios),
                                 uiOutput("scenario2"),
                                 # selectInput("scope2", "Choose a Scope:",
                                 #             choices = Scopes),
                                 uiOutput("scope2"),
                                 # selectInput("period2", "Choose a Period:",
                                 #             choices = Periods),
                                 uiOutput("period2"),
                                 # selectInput("interface2", "Choose an Interface:",
                                 #             choices = Interfaces)
                                 uiOutput("InterfacesChoice2"),
                                 br(),
                                 p("This bar plot shows the contribution of the selected interfaces ( eg: water, LU..etc) to each System. Scenario Scope and Period needs to be chosen aswell ")
                                 
                               ),
                               mainPanel(
                                 plotOutput("PiePlotSystem")
                               )
                             )
                   )
                   
                   #end of Tab
                   
                   #New tab with pie chart  Processots-Interfaces -----
                   ,tabPanel("Bar Chart by Processors",
                             sidebarLayout(
                               sidebarPanel(
                                 p("This plot shows the contribution of the selected interfaces ( eg: water, LU..etc) comparing a group of processor shoosen by the user. Scenario Scope and Period needs to be chosen aswell "),
                                 
                                 # 
                                 #                 selectInput("scenario3", "Choose a Scenario:",
                                 #                             choices = Scenarios),
                                 #                 selectInput("scope3", "Choose a Scope:",
                                 #                             choices = Scopes),
                                 #                 selectInput("period3", "Choose a Period:",
                                 #                             choices = Periods),
                                 #                 selectInput("interface3", "Choose an Interface:",
                                 #                             choices = Interfaces),
                                 #                 checkboxGroupInput("ProcessorsChoice", "Processors to compare:",
                                 #                                    choiceNames = Processors, choiceValues = Processors, selected = Processors[1])
                                 uiOutput("scenario3"),
                                 uiOutput("scope3"),
                                 uiOutput("period3"),
                                 uiOutput("InterfacesChoice3"),
                                 uiOutput("ProcessorsChoice")
                                 
                                 
                               ),
                               mainPanel(
                                 plotOutput("PiePlotProcessors")
                               )
                             )
                   )
                   
                   # end of Tab
                   
                   
                   
                   
                   
                   
                   # New tab Var Chart (not in use) ----
                   # ,tabPanel("Plot",
                   #     sidebarLayout(
                   #       sidebarPanel(
                   #         selectInput("scenario", "Choose a Scenario:",
                   #                     choices = Scenarios),
                   #         selectInput("period", "Choose a Period:",
                   #                     choices = Periods),
                   #         selectInput("processor", "Choose a Processor:",
                   #                     choices = Processors),
                   #         checkboxInput("log_10","apply log10 to data",
                   #                       value = FALSE)
                   #       ),
                   #
                   #       #TODO l should be variable with the filter
                   #       #     checkboxGroupInput("checkGroup", label = h3("Interfaces"),
                   #       #                        choices = l,
                   #       #                        selected = names(l))
                   #       # ),
                   #
                   #
                   #       # Create a spot for the barplot
                   #       mainPanel(
                   #         plotOutput("BarPlot")
                   #         )
                   #       )
                   #   )
                   # End of Second tab
                   
                   # New tab EUM/EPM excelformat -----
                   #
                   #TODO use fluid row https://shiny.rstudio.com/gallery/basic-datatable.html
                   ,tabPanel("EUM/EPM Matrix",
                             sidebarLayout(
                               sidebarPanel(
                                 p("by choosing The interface types (flow types) to show,the fund interface type   and tying population a End use matrix or Enviroment Pressure Matrix will be show as an excel table"),
                                 numericInput("Population", "Population", 100000),
                                 # selectInput("FundInterface", "Choose a Fund InterfaceType:",
                                 #             choices = FundInterfaces,selected = FundInterfaces[1]),
                                 # selectInput("ScopeChoice", "Choose a Scope:",
                                 #             choices = Scopes, selected = Scopes[1]),
                                 # selectInput("ScenarioChoice", "Choose a Scenario:",
                                 #             choices = Scenarios),
                                 # selectInput("PeriodChoice", "Choose a Period:",
                                 #             choices = Periods, selected = Periods[length(Periods)]),
                                 # selectInput("SystemChoice", "Choose a System:",
                                 #             choices = Systems, selected = Systems[1]),
                                 # checkboxGroupInput("show_Interfaces", "Choose a flow InterfaceType to show:",
                                 #                    choiceNames = FlowInterfaces, choiceValues = FlowInterfaces, selected = Interfaces[1])
                                 
                                 uiOutput("FundInterface"),
                                 uiOutput("ScopeChoice"),
                                 uiOutput("ScenarioChoice"),
                                 uiOutput("PeriodChoice"),
                                 uiOutput("SystemChoice"),
                                 uiOutput("show_Interfaces"),
                                 p("by choosing the indicaror and level the user will be able to compare the same indicator in that level. Only indicators shoosen in EUM tab will be able to choose. The use can aswell customize zones in the gauge plot ")
                                 # TODO los funds también tendrían que ser checkboxs
                                 
                               ),
                               
                               mainPanel(
                                 # tableOutput("eum") # FORMATO 1
                                 # DT::dataTableOutput("eum") # FORMATO 2
                                 excelOutput("eum") #FORMATO EXCEL
                                 # renderRpivotTable("eum") #TODO donsn't work
                                 
                               )
                             )
                   ) #end 
                   
                   
                   # New tab with indicators Barplot Internal vs external  -----
                   
                   #TODO use fluid row https://shiny.rstudio.com/gallery/basic-datatable.html
                   ,tabPanel("EUM/EPM bar chart",
                             sidebarLayout(
                               sidebarPanel(
                                 p("by choosing The interface types (flow types) to show,the fund interface type   and tying population a End use matrix or Environment Pressure Matrix will be show as an excel table"),
                                 uiOutput("indicator2"),
                                 uiOutput("LevelIndicator2"),
                                 uiOutput("SubSystemChoice2")
                               ),
                               
                               mainPanel(
                                 plotOutput("ExternalInternalPlot")
                               )
                             )
                   ) #end 
                   
                   
                   # New tab with temporal study of indicators  -----
                   
                   # #TODO use fluid row https://shiny.rstudio.com/gallery/basic-datatable.html
                   # ,tabPanel("EUM/EPM bar chart",
                   #           sidebarLayout(
                   #             sidebarPanel(
                   #               p("by choosing The interface types (flow types) to show,the fund interface type   and tying population a End use matrix or Environment Pressure Matrix will be show as an excel table"),
                   #               uiOutput("ind1"),
                   #               uiOutput("ind2"),
                   #               uiOutput("ind3")
                   #             ),
                   #             
                   #             mainPanel(
                   #               plotOutput("temporal")
                   #             )
                   #           )
                   # ) #end 
                   
                   
                   
                   # New tab with indicators plot (not working) -----
                   
                   #TODO use fluid row https://shiny.rstudio.com/gallery/basic-datatable.html
                   # ,tabPanel("INDICATORS",
                   #           sidebarLayout(
                   #             sidebarPanel(
                   #               p("by choosing The interface types (flow types) to show,the fund interface type   and tying population a End use matrix or Environment Pressure Matrix will be show as an excel table"),
                   #               
                   #               uiOutput("indicator"),
                   #               uiOutput("LevelIndicator"),
                   #               numericInput("break2", "red Zone", 30),
                   #               numericInput("break3", "Yellow Zone", 70)
                   #             ),
                   #             
                   #             mainPanel(
                   #               plotOutput("gaugePlot", width = "100%")
                   #             )
                   #           )
                   # ) #end 
                   
                   
                   #New tab with editable EUM (working but missing implementation)------
                   
                   # ,tabPanel("Editable Indicators",
                   #           sidebarLayout(
                   #             sidebarPanel(
                   #               actionButton("saveData","Save")
                   #             ),
                   #             
                   #             mainPanel(
                   #               rHandsontableOutput("editableTable")
                   #             )
                   #           )
                   # ) #end 

                   
                   
                   
                   
                   # box plot (no in use) ----
                   # ,tabPanel("boxplot",
                   #           sidebarLayout(
                   #             sidebarPanel(
                   #               numericInput("Population", "Population", 100000),
                   #               selectInput("FundInterface", "Choose a Fund InterfaceType:",
                   #                           choices = FundInterfaces,selected = FundInterfaces[1]),
                   #               selectInput("ScopeChoice", "Choose a Scope:",
                   #                           choices = Scopes, selected = Scopes[1]),
                   #               selectInput("ScenarioChoice", "Choose a Scenario:",
                   #                           choices = Scenarios),
                   #               selectInput("PeriodChoice", "Choose a Period:",
                   #                           choices = Periods, selected = Periods[length(Periods)]),
                   #               selectInput("SystemChoice", "Choose a System:",
                   #                           choices = Systems, selected = Systems[1]),
                   #               checkboxGroupInput("show_Interfaces", "Choose a flow InterfaceType to show:",
                   #                                  choiceNames = FlowInterfaces, choiceValues = FlowInterfaces, selected = Interfaces[1]),
                   #               checkboxGroupInput("ProcessorsChoice", "Processors to compare:",
                   #                                  choiceNames = Processors, choiceValues = Processors, selected = Processors[1]),
                   #               selectInput("Indicatorshow", "Choose a System:",
                   #                           choices = Indicators, selected = Indicators[1]),
                   #             ),
                   #
                   #             mainPanel(
                   #
                   #               plotOutput("boxplot")
                   #             )
                   #           )
                   # )
                   
                   
                   
                   # New tab tree (Working but Interface Tree already in use) ----
                   # ,tabPanel("Tree",
                   #           sidebarLayout(
                   #             sidebarPanel(
                   #               # selectInput("Scope2", "Choose a Scope:",
                   #               #             choices = Scopes),
                   #               # selectInput("Period2", "Choose a Period:",
                   #               #             choices = Periods)
                   #               uiOutput("ScopeTree"),
                   #               uiOutput("PeriodTree"),
                   #               br(),
                   #               p("this tree plot shows processor hierarchy")
                   #             ),
                   #             
                   #             mainPanel(
                   #               collapsibleTreeOutput("Tree")
                   #             )
                   #           )
                   # ) #end tab
                   
                   
                   # New tab tree with quantities ----
                   ,tabPanel("Hierarchy & values Viewer",
                             sidebarLayout(
                               sidebarPanel(
                                 # selectInput("Scope", "Choose a Scope:",
                                 #             choices = Scopes),
                                 # selectInput("Period", "Choose a Period:",
                                 #             choices = Periods),
                                 # selectInput("Interface", "Choose an Interface:",
                                 #             choices = Interfaces),
                                 h3(textOutput("Unit")),
                                 uiOutput("ScopeTree2"),
                                 uiOutput("PeriodTree2"),
                                 uiOutput("InterfaceTree2"),
                                 #uiOutput("UnitTree2")
                                 br(),
                                 p("this tree plot shows processor hierarchy and by choosing interface type the quantities will be represented by the size of nodes. if hover the values will be displayed")
                               ),
                               
                               mainPanel(
                                 collapsibleTreeOutput("TreeInterface")
                               )
                             )
                   ) #end tab                   
                                      
))#VERY END