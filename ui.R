
shinyUI(navbarPage("Nis-EDA", id = "nav", inverse = TRUE,
                   
                   # New tab with file input and table display -----
                   tabPanel("Nis File ",
                            sidebarLayout(
                              sidebarPanel(
                                # fileInput('target_upload', 'Choose file to upload',
                                          # accept =".xlsx"),
                                NisOutputUI('input'),
                                # actionButton("act","click to update dataset input")
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
                   
                   # New tab tree with quantities ----
                   ,tabPanel("Hierarchy Viewer",
                             sidebarLayout(
                               sidebarPanel(
                                 p("This view of the results allows the user to relate quantities to processors
                                   according to hierarchy for a scenario, period and scope. The visualization provides an
                                   interactive display of the results by hovering over the processors"),

                                 
                                 ChoicesTreeUI(id = 'tree'),

                               ),
                               mainPanel(
                                 treeUI(id = 'tree', StringName = 'TreeInterface' )
                               )
                             )
                   ) #end tab

                   ,tabPanel("Level Exploration ",
                             sidebarLayout(
                               sidebarPanel(
                                 ChoosedfUI(id = 'Scope'),
                                 barPlotChoicesUI(id = 'Scope'),
                                 br(),
                                 p("In this window, the user must choose System, Period, level and the set of interface
                                 types with the same unit to be compared.
                                 The bar chart shows stacked interfaces types values by scope displaying in transparent
                                 colour the quantity externalized.")
                               ),
                               mainPanel(
                                 barPlotUI(id = 'Scope',stringName = 'barPlot')
                               )
                             )
                   )
                   #New tab with pie chart  Processots-Interfaces -----
                   ,tabPanel("Processors Exploration",
                             sidebarLayout(
                               sidebarPanel(
                                 ChoosedfUI(id = 'processor'),
                                 barPlotChoicesMultiProcessors(id = 'processor'),
                                 br(),
                                 p("For a more customized study, This tab allows the user to freely
                                   choose which processors to study with a multi select input of interfaces and processors.")
                               ),
                               mainPanel(
                                 barPlotUI(id = 'processor',stringName = 'barPlot')
                               )
                             )
                   )#end of Tab
                   ,tabPanel("System Study",
                             sidebarLayout(
                               sidebarPanel(
                                 ChoosedfUI(id = 'System'),
                                 barPlotChoicesUI(id = 'System'),
                                 br(),
                                 p("This tab allows to compare between different systems and study
                                   the externalization of the interface type chosen. The user has the
                                   possibility to choose the scenario, period and Interface to Study.")
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
                   ,tabPanel("Indicator Bar Chart",
                             sidebarLayout(
                               sidebarPanel(
                                 p("After creating the indicators the following screen allows a study of these
                                   by level and period visualizing the scope by means of a stacked bar chart."),
                                 EUMPlotChoicesUI(id = 'EUMplot')
                               ),

                               mainPanel(
                                 barPlotUI(id = 'EUMplot',stringName = 'barPlot')
                               )
                             )
                   ) #end

                   ,tabPanel("Benchmarks Creation",
                             sidebarLayout(
                               sidebarPanel(

                                GaugeInputsUI('gauge'),

                                 downloadButton("dl", "Download"),

                                 actionButton("addIndBench", "An action button")


                               ),
                               mainPanel(
                                 barPlotUI(id = 'gauge' , stringName = 'gaugePlot')
                               )
                          )
                   )#end

                                      
  )
)#VERY END