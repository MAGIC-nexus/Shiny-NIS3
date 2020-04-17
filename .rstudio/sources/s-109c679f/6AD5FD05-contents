
rm(list = ls())
# setwd("~/Documentos/Shiny/Shiny-NIS2")
library(dplyr)
library(ggplot2)
library(stringr)
library(rlist)
library(dplyr)
library(tidyr)
library("readxl")
library("collapsibleTree")
library(data.table)
library(excelR)
library("DT")
library(flexdashboard)
library(stringr)
library(reticulate)
# library("renderRHandsontable")
use_python("/opt/conda/bin/python3", required = TRUE)
nexinfosys <- import("nexinfosys")
pandas<-import("pandas")
py_config()
py_discover_config()
# c <- nexinfosys$NISClient("http://0.0.0.0:5000/nis_api")

gg.gauge <- function(eum,breaks=values,colour) {
  # rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100
  # rescale<-function(x) x*1000
  require(ggplot2)
  get.poly <- function(a,b,l = breaks[4],r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/l)
    th.end   <- pi*(1-b/l)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  l <- breaks[length(breaks)]
  eumind<-eum["Value"]
  pos<-round(as.vector(unlist(eumind)),digits = 3)
  posper<-pos
  breakscale<-breaks
  g<- ggplot()+
    geom_polygon(data=get.poly(breakscale[1],breakscale[2],l=l),aes(x,y),fill=colour[1])+
    geom_polygon(data=get.poly(breakscale[2],breakscale[3],l=l),aes(x,y),fill=colour[2])+
    geom_polygon(data=get.poly(breakscale[3],breakscale[4],l=l),aes(x,y),fill=colour[3])
  for (b in posper){
    g<- g + geom_polygon(data=get.poly(b-l*0.005,b+l*0.005,0.02,l =l),aes(x,y))
  }
  g +
    # Breaks Label:
    # geom_text(data=as.data.frame(breaks), size=3, fontface="bold", vjust=0,
    #            aes(x=1.1*cos(pi*(1-breaks/l)),y=1.1*sin(pi*(1-breaks/l)),label=paste0(breaks,"%")))+
    # Indicators Label:
    geom_text(data=as.data.frame(posper), size=4, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-posper/l)),y=1.1*sin(pi*(1-posper/l)),label=paste(eum$Processor,pos, sep = ":")))+
    # annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank())
}
