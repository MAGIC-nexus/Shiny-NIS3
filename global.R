rm(list = ls())

source('library.R')



use_python("/opt/conda/bin/python3", required = TRUE)
nexinfosys <- import("nexinfosys")
pandas<-import("pandas")
source('plots.R')
source('barPlot.R')
source('Choices.R')
source('EUM.R')
source('tree.R')
source('indicators.R')

# py_config()
# py_discover_config()