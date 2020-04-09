# cd <this Dockerfile directory>
# docker build -t magic-shiny .
# Test run:
# docker run -it --rm -p 8081:3838 -v /home/rnebot/DATOS/docker/shinyApps:/srv/shiny-server magic-shiny

# Open browser: "http://localhost:8081"
#

FROM rocker/shiny

MAINTAINER Rafael Nebot <rnebot@itccanarias.org>

RUN apt-get -y update && apt-get -y upgrade && \
    apt-get -y install \
    git \
    make \
    gcc \
    build-essential \
    software-properties-common gnupg \
    libxml2-dev \
    unzip && \
    apt-get clean

RUN echo 'export PATH=/opt/conda/bin:$PATH' > /etc/profile.d/conda.sh && \
    wget --quiet https://repo.anaconda.com/archive/Anaconda3-2019.10-Linux-x86_64.sh -O ~/anaconda.sh && \
    /bin/bash ~/anaconda.sh -b -u -p /opt/conda && \
    rm ~/anaconda.sh

RUN install2.r --error --repos='http://cran.rstudio.com/' \
    dplyr \
    ggplot2 \
    stringr \
    tidyr \
    readxl \
    reticulate \
    flexdashboard \
    data.table \
    excelR \
    DT \
    stringr \
    collapsibleTree \
    rlist \
    rhandsontable \
    shinyjs

ENV PATH /opt/conda/bin:$PATH
ENV RETICULATE_PYTHON /opt/conda/bin/python3

RUN pip install npm webdavclient nexinfosys-client

