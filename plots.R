#' Draq a gauge plot
#' @param eum is the input dataset with indicators 
#' @param breaks describes ranges
#' @param colours specified colors per range in the same order



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



#' Draw a plot that stacks Internal and external scope of interfaces, and group interfaces if they have same units 
#' @param df is a dataset object with interfaces, processors, value and scope information at least

StackedplotBarExtInt <- function(df){
  UnitList<- unique(df$Unit)
  validate(
    need(length(UnitList)==1, "Your interface selection should have the same unit") 
  )
  # barchart <- ggplot (df, aes( x = Processor ,  y = Value, fill = Interface)) 
  
  dfInt = df[which(df$Scope == 'Internal'),]
  names(dfInt)[names(dfInt)=='Interface']<-'Interface_Internal'
  dfExt = df[which(df$Scope == 'External'),]
  names(dfExt)[names(dfExt)=='Interface']<-'Interface_External'
  
    # TODO no consigo que salga la leyenda de external...... 
    # código https://stackoverflow.com/questions/38070878/r-stacked-grouped-barplot-with-different-fill-in-r
    barchart<-ggplot() +
      geom_bar(data = dfInt, aes( x = Processor ,  y = Value, fill = Interface_Internal), position="dodge", stat = "identity", show.legend = TRUE) +
      theme(legend.position = 'botton')+
      geom_bar(data = dfExt, aes( x = Processor ,  y = Value, fill = Interface_External), position="dodge", stat = "identity",alpha=0.5, show.legend = TRUE, inherit.aes = TRUE) +
      theme(legend.position = 'top')+
      labs(title = "Inrterface value", y = unique(df$Unit)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(barchart)
}

#'Draw a plot that  group interfaces if they have same units 
#' @param df is a dataset object with Interfaces, Processors, Value information.

StackedplotBar<-function(df){
  UnitList<- unique(df$Unit)
  df$per<-round(df$Value/sum(df$Value)*100, digits = 3)
  df$names_per <-paste(df$Processor,df$per,"%", sep = " ")
  validate(
    need(length(UnitList)==1, "Your interface selection should have the same unit") 
  )
  barchart <- ggplot (df, aes( x = Processor ,  y = Value, fill = Interface)) + geom_bar( position="dodge", stat = "identity") + 
    labs(title = "Inrterface value", y = unique(df$Unit)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(barchart)
}










