setwd("~/Documentos/Shiny/Shiny-NIS2")

library("reticulate")
reticulate::use_python("/home/paula/anaconda3/bin/python", required = TRUE)
nexinfosys <- import("nexinfosys")
c <- nexinfosys$NISClient("http://0.0.0.0:5000/nis_api")     
fname <- "Netherlandsv1ToNIS.xlsx"
c$login("test_user")
print("Logged in")
c$open_session()
print("Session opened")
n <- c$load_workbook(fname, "NIS_agent", "NIS_agent@1")
print(paste("N worksheets: ",n))
r <- c$submit()
print("Returned from submit")
r <- c$query_available_datasets()
ds <- c$query_datasets(c(tuple("flow_graph_solution", "csv", "dataframe")))
df <- py_to_r(ds[[1]][[3]])


df<-subset(df,select = -c(Conflict_Partof,Conflict_Itype,Computed,Expression))
df$Value<-as.numeric(lapply(df$Value,str_replace ,pattern = ",",replacement = "."))


data<-datos
data$Period<-as.numeric(data$Period)
Scenarios <- as.vector(unique(data$Scenario))
Periods<- as.vector(unique(data$Period))
Processors<- as.vector(unique(data$Processor))
Interfaces<- as.vector(unique(data$Interface))
Scopes<- as.vector(unique(data$Scope))
Level <-as.vector(unique(data$Level))
Systems<- as.vector (unique(data$System))
Subsystems<-as.vector(unique(data$Subsystem))
Flow<- filter(data, data$RoegenType == "flow" | data$RoegenType == "Flow")
Fund<- filter(data, data$RoegenType == "Fund" | data$RoegenType == "fund")
FlowInterfaces<- as.vector(unique(Flow$Interface))
FundInterfaces <- as.vector(unique(Fund$Interface))


df <- filter(data,data$Scenario == Scenario[1] & data$Period == Period[1]  & data$Interface == Interface[1], data$Scope == Scope[1])

df$per<-round(df$Value/sum(df$Value)*100, digits = 3)
df$names_per <-paste(df$Processor,df$per,"%", sep = " ")
plt <- ggplot (df, aes( x = "" ,  y = Value, fill = names_per)) + geom_bar(width = 1, stat = "identity")
pie <- plt + coord_polar("y", start=0)

pie

c$close_session()
c$logout()