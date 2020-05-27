 # NIS EDA

NIS EDA is a dashboard designed to allow initial exploration of results of a NIS-formatted MuSIASEM case study.

It is a Shiny application which can be deployed using Docker images built using one of the two supplied Dockerfiles: one is for continuation of the development, RStudio+Shiny (rstudio_docker/Dockerfile), the other is for execution, containing just Shiny (Dockerfile).

## Connection to Nis-Backend
Connection to a Nis-Backend instance running locally (at 127.0.0.1) is provided as example. See  https://github.com/MAGIC-nexus/nis-backend to see how to install "nis-backend". To use other connections (as Unina server) change the URL in "nisbackend.R" file.

### Files and Modules Organization

- library.R
- nisbackend.R
- README.md
- global.R
- server.R
- ui.R
- modules/
  - barPlots.R
  - EUM.R
  - tree.R
  - plots.R
  - indicators.R
  - Choices.R
  - NISInput.R

### Modules Description

- barPlots.R: Contains all the modules related to the bar graphs.
- Choices.R : Modules that build the selectors such as scope, period, scenario, etc. that are repeated throughout the application to perform the filterings.
- EUM.R: Modules that build the necessary reactives datasets to create the EUM matrix
- indicators.R: Inputs and outputs necessary to create the scalarBenchmark, Scalar indicator  and gauge graph output.
- tree.R: Modules to create the hierarchy visualization
- plots.R: Functions to create the different charts such as bar charts and gauge plot
- NISInput.R Function that calls NIS Client and generates Flow Graph Solution and Issues Data frames.

