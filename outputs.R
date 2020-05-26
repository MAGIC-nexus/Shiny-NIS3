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
