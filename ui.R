
shinyUI(navbarPage("MuSIASEM data visualizations", id = "nav", inverse = TRUE,
                   
                   # New tab with file input and table display -----
                   tabPanel("INPUT FILE ",
                            sidebarLayout(
                              sidebarPanel(
                                # fileInput('target_upload', 'Choose file to upload',
                                #           accept =".xlsx"
                                #           ),
                                actionButton("act","click to update dataset input")
                                #TODO controlar que estos mensajes salgan antes de que salga la tabla
                                # textOutput("num_errors"),
                                # textOutput("opened"),
                                # textOutput("worksheets")
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Flow graph Solution",DT::dataTableOutput("FGS")),
                                  tabPanel("Issues",DT::dataTableOutput("issues"))
                                )
                              )
                            )
                    )
                   
                   
                   #end of Tab
                   ,tabPanel("Bar chart by level Multi Interface",
                             sidebarLayout(
                               sidebarPanel(
                                 barPlotChoicesUI(id = 'first'),
                                 br(),
                                 p("This bar plot shows the contribution of the selected interfaces ( eg: water, LU..etc) to each processor of the specified level. Scenario Scope and Period needs to be chosen aswell ")
                               ),
                               mainPanel(
                                 barPlotUI(id = 'first',stringName = 'barPlot')
                               )
                             )
                   )
                   ,tabPanel("Bar chart by level Multi Interface Scope study ",
                             sidebarLayout(
                               sidebarPanel(
                                 barPlotChoicesUI(id = 'Scope'),
                                 br(),
                                 p("This bar plot shows the contribution of the selected interfaces ( eg: water, LU..etc) to each processor of the specified level. Scenario Scope and Period needs to be chosen aswell ")
                               ),
                               mainPanel(
                                 barPlotUI(id = 'Scope',stringName = 'barPlot')
                               )
                             )
                   )
                   #New tab with pie chart  Processots-Interfaces -----
                   ,tabPanel("Bar chart free choice processor",
                             sidebarLayout(
                               sidebarPanel(
                                
                                 barPlotChoicesMultiProcessors(id = 'processor'),
                                 br(),
                                 p("This bar plot shows the contribution of the selected interfaces ( eg: water, LU..etc) to each processor of the specified level. Scenario Scope and Period needs to be chosen aswell ")
                               ),
                               mainPanel(
                                 barPlotUI(id = 'processor',stringName = 'barPlot')
                               )
                             )
                   )#end of Tab
                   ,tabPanel("Study of the openess of each System",
                             sidebarLayout(
                               sidebarPanel(
                                 barPlotChoicesUI(id = 'System'),
                                 br(),
                                 p("This bar plot shows the contribution of the selected interfaces ( eg: water, LU..etc) to each processor of the specified level. Scenario Scope and Period needs to be chosen aswell ")
                               ),
                               mainPanel(
                                 barPlotUI(id = 'System',stringName = 'barPlot')
                               )
                             )
                   )
                   ,tabPanel("EUM/EPM Matrix",
                             sidebarLayout(
                               sidebarPanel(
                                 p("by choosing The interface types (flow types) to show,the fund interface type   and tying population a End use matrix or Enviroment Pressure Matrix will be show as an excel table"),
                                 numericInput("Population", "Population", 100000),
                                 EUMChoicesUI(id = 'EUM'),
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
                                 EUMPlotChoicesUI(id = 'EUMplot')
                               ),
                               
                               mainPanel(
                                 barPlotUI(id = 'EUMplot',stringName = 'barPlot')
                               )
                             )
                   ) #end 

                   ,tabPanel("INDICATORS",
                             sidebarLayout(
                               sidebarPanel(
                   
                                GaugeInputsUI('gauge'),
                                 
                                 downloadButton("dl", "Download")
                                 
                                 # actionButton("addCommands", "An action button")
                                
                               
                               ),
                               mainPanel(
                                 barPlotUI(id = 'gauge' , stringName = 'gaugePlot')
                               )
                          )
                   )#end
                   
                   # New tab tree with quantities ----
                   ,tabPanel("Hierarchy & values Viewer",
                             sidebarLayout(
                               sidebarPanel(
                                 h3(textOutput("Unit")),
                                 ChoicesTreeUI(id = 'tree'),
                                 br(),
                                 p("this tree plot shows processor hierarchy and by choosing interface type the quantities will be represented by the size of nodes. if hover the values will be displayed")
                               ),
                               
                               mainPanel(
                                 treeUI(id = 'tree', StringName = 'TreeInterface' )
                               )
                             )
                   ) #end tab                   
                                      
  )
)#VERY END