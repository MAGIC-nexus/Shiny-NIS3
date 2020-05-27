 # NIS EDA

NIS EDA is a dashboard designed to allow initial exploration of results of a NIS-formatted MuSIASEM case study.

It is a Shiny application which can be deployed using Docker images built using one of the two supplied Dockerfiles: one is for continuation of the development, RStudio+Shiny (rstudio_docker/Dockerfile), the other contains just Shiny (Dockerfile)).

## Connection to Nis-Backend
Connection to Nis-Backend is ready for a local installation see  https://github.com/MAGIC-nexus/nis-backend to use other connections (as Unina server) change nisbackend files

### Files and Modules Organization

- nis-EDA
- Library.R
- nisbacked.R
- README.R
- global.R
- Server.R
- ui.R
- Modules
  - barPlots.R
  - EUM.R
  - tree.R
  - plots.R
  - indicators.R
  - Choices.R
  - NiSInput.R

### Modules Description

barPlots.R: Contains all the modules related to the bar graphs.
Choices.R : Modules that build the selectors such as scope, period, scenario, etc. that are repeated throughout the application to perform the filterings.
EUM.R: Modules that build the necessary reactives datasets to create the EUM matrix
indicators.R: Inputs and outputs necessary to create the scalarBenchmark, Scalar indicator  and gauge graph output.
tree.R: Modules to create the hierarchy visualization
plots.R: Functions to create the different charts such as bar charts and gauge plot
NISInput.R Function that calls NIS Client and generates Flow Graph Solution and Issues Data frames.

