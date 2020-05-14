rm(list = ls())
options(shiny.reactlog=TRUE)
source('library.R')



use_python("/opt/conda/bin/python3", required = TRUE)
nexinfosys <- import("nexinfosys")
pandas<-import("pandas")
source('plots.R')
source('barPlot.R')

# py_config()
# py_discover_config()