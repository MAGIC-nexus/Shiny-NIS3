# cd <this Dockerfile directory>
# docker build -t magic-rstudio .
# Create and start container:
# docker create --name rstudio_local -p 3838:3838 -p 8787:8787 -e DISABLE_AUTH=true -e ADD=shiny -v /home/rnebot/DATOS/docker/shinyApps:/home/rstudio magic-rstudio
# docker start rstudio_local && docker logs rstudio_local -f
# docker stop rstudio_local && docker rm rstudio_local
# Test run:
# docker run -it --rm -p 3838:3838 -p 8787:8787 -e DISABLE_AUTH=true -e ADD=shiny -v /home/rnebot/DATOS/docker/shinyApps:/home/rstudio magic-rstudio

# Open browser: "http://localhost:8787"
#

FROM rocker/rstudio

MAINTAINER Rafael Nebot <rnebot@itccanarias.org>

# TODO Install Python
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
    /bin/bash ~/anaconda.sh -b -p /opt/conda && \
    rm ~/anaconda.sh

RUN install2.r --error --repos='http://cran.rstudio.com/' \
    dplyr \
    shiny \
    ggplot2 \
    stringr \
    tidyr \
    readxl \
    reticulate \
    flexdashboard \
    excelR \
    DT	

RUN apt-get -y install zlib1g-dev

RUN install2.r --error --repos='http://cran.rstudio.com/' \
    data.table \
    stringr \
    collapsibleTree \
    rlist \
    writexl

ENV PATH /opt/conda/bin:$PATH
ENV RETICULATE_PYTHON /opt/conda/bin/python3
RUN pip install npm webdavclient nexinfosys-client

