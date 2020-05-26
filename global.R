rm(list = ls())

source('library.R')



use_python("/opt/conda/bin/python3", required = TRUE)
# use_condaenv('NIS')
nexinfosys <- import("nexinfosys")
# pandas<-import("pandas")

# source shiny module files
invisible(sapply(list.files('modules', full.names = TRUE), source))




options(shiny.reactlog = TRUE)

# py_config()
# py_discover_config()