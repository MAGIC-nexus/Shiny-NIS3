rm(list = ls())

source('library.R')


use_python("/opt/conda/bin/python3", required = TRUE)
nexinfosys <- import("nexinfosys")
pandas<-import("pandas")

# source shiny module files
invisible(sapply(list.files('modules', full.names = TRUE), source))
source('nisbackend.R')


