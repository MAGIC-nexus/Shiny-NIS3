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
                                #TODO controlar que estos mensajes salgan antes de que salga la tabla
                                textOutput("num_errors"),
                                textOutput("opened"),
                                textOutput("worksheets")
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Flow graph Solution",DT::dataTableOutput("sample_table")),
                                  tabPanel("Issues",DT::dataTableOutput("issues"))
                                )

                              )
                            )
                   )
                   #end of Tab
                
                   # New tab with pie chart Levels-Interfaces ----
                   ,tabPanel("Bar chart by level",
                             sidebarLayout(
                               sidebarPanel(
                               
                                 uiOutput('scenario'),
                           
                                 uiOutput('scope'),
                          
                                 uiOutput('period'),
                            
                                 uiOutput('level'),
                            
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
                   
                   # # New tab with pie chart Systems-Interfaces -----
                   # ,tabPanel("Bar chart by System",
                   #           sidebarLayout(
                   #             sidebarPanel(
                   #         
                   #               uiOutput("scenario2"),
                   #           
                   #               uiOutput("scope2"),
                   #           
                   #               uiOutput("period2"),
                   #             
                   #               uiOutput("InterfacesChoice2"),
                   #               br(),
                   #               p("This bar plot shows the contribution of the selected interfaces ( eg: water, LU..etc) to each System. Scenario Scope and Period needs to be chosen aswell ")
                   #               
                   #             ),
                   #             mainPanel(
                   #               plotOutput("PiePlotSystem")
                   #             )
                   #           )
                   # )
                   # 
                   # #end of Tab
                   
                   #New tab with pie chart  Processots-Interfaces -----
                   ,tabPanel("Bar Chart by Processors",
                             sidebarLayout(
                               sidebarPanel(
                                 p("This plot shows the contribution of the selected interfaces ( eg: water, LU..etc) comparing a group of processor shoosen by the user. Scenario Scope and Period needs to be chosen aswell "),
                        
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
                   
  
                   
                   # New tab EUM/EPM excelformat -----
                   #
                   #TODO use fluid row https://shiny.rstudio.com/gallery/basic-datatable.html
                   ,tabPanel("EUM/EPM Matrix",
                             sidebarLayout(
                               sidebarPanel(
                                 p("by choosing The interface types (flow types) to show,the fund interface type   and tying population a End use matrix or Enviroment Pressure Matrix will be show as an excel table"),
                                 numericInput("Population", "Population", 100000),
                                 
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
                   
                   
                   
                   # New tab with gauge plot -----
                   
                   #TODO use fluid row https://shiny.rstudio.com/gallery/basic-datatable.html
                   ,tabPanel("INDICATORS",
                             sidebarLayout(
                               sidebarPanel(
                                 p("by choosing The interface types (flow types) to show,the fund interface type   and tying population a End use matrix or Environment Pressure Matrix will be show as an excel table"),
                                 fluidRow(
                                   column(4,style=list("padding-right: 3px;"), 
                                 uiOutput("indicator")),
                                 
                                 column(4,style=list("padding-right: 3px;"),
                                 uiOutput("LevelIndicator")),
                                 
                                 column(4,style=list("padding-right: 3px;"),
                                 uiOutput("ScopeIndicator")),
                                 
                                 column(4,style=list("padding-right: 3px;"),
                                        uiOutput("PeriodIndicator"))
                                ),
                                
                                fluidRow(
                                  column(4,style=list("padding-right: 3px;"),
                                         textInput("IndName", "Ind Name",placeholder = "Ind Name")),
                                  column(4,style=list("padding-right: 3px;"),
                                         selectInput("BechmarkGroup","Bechmark Group", choices = c("Feasibility", "Viability", "Desirability",""), selected = "")),
                                  column(4,style=list("padding-right: 3px;"),
                                         textInput("Description", "Description",placeholder = "Short description"))
                                  
                                  ),
                                 
                                 h3("Zone 1"),
                                 #ZONE 1
                                 fluidRow(
                                  column(6,style=list("padding-right: 3px;"),
                                 textInput("ZoneName1", "Category",placeholder = "Low")),
                                 
                                 column(6,style=list("padding-right: 3px;"),
                                 selectInput("colour1","colour", choices = c("red","gold","forestgreen"), selected = "red")), 
                                 
                                 column(3,style=list("padding-right: 3px;"),
                                 numericInput("break1", "Min", 0)),
                                 
                                 column(6,style=list("padding-right: 3px;"),
                                 numericInput("break2", "break 2", 0.1)),
                                 
                                 column(3,style=list("padding-right: 3px;"),
                                        selectInput("Include1", "",choices = c(")","]"),selected = "("))
                                 ),
                                 
                                 br(),
                                 h3("Zone  2"),
                                 fluidRow(
                                  column(6,style=list("padding-right: 3px;"),
                                 textInput("ZoneName2", "Category",placeholder = "Medium")),
                                 
                                 column(6,style=list("padding-right: 3px;"),
                                 selectInput("colour2","colour", choices = c( "red","gold","forestgreen"),selected = "gold")), 
                                 
                                 column(3,style=list("padding-right: 3px;"),
                                        selectInput("Include2","",choices = c("(","["),selected = "(")),
                                 
                                 column(6,style=list("padding-right: 3px;"),
                                 numericInput("break3", "break3", 2)),
                                
                                 
                                 column(3,style=list("padding-right: 3px;"),
                                        selectInput("Include3","",choices = c(")","]"),selected = "]"))
                                 ),
                                 
                                 p(), #TODO arreglar línea
                                 h3("Zone  3"),
                                 fluidRow(
                                  column(6,style=list("padding-right: 3px;"),
                                 textInput("ZoneName3", "Category",placeholder = "Max")),
                                 
                                 column(6,style=list("padding-right: 3px;"),
                                 selectInput("colour3","colour", choices = c( "red","gold","forestgreen"),selected = "forestgreen")),
                                 
                                 column(6,style=list("padding-right: 3px;"),
                                        selectInput("Include4", "",choices = c("(","["),selected = "(")),
                                 
                                 column(6,style=list("padding-right: 3px;"),
                                 numericInput("break4", "Max", 2.5))
                               ),
                               
                               downloadButton("dl", "Download"),
                               actionButton("addCommands", "An action button")
                               ),
                               
                               mainPanel(
                                 plotOutput("gaugePlot", width = "100%")
                               )
                             )
                   )#end
                   
                   
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