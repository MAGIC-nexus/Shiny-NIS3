
NisOutputUI<-function(id){
  
  ns<-NS(id)
  
  fileInput(ns('target_upload'), 'Choose file to upload',
            accept =".xlsx")
}



NisOutput<-function(input,output,session){

  NisClient <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    filename <- inFile$datapath
    # c <- nexinfosys$NISClient("https://one.nis.magic-nexus.eu/nis_api")
    c <- nexinfosys$NISClient("http://172.17.0.1:5000/nis_api")
    fname <- filename
    try({print(c$close_session())
      print(c$logout())
    }, silent = FALSE)
    print(c$login("test_user"))
    c$open_session()
    n <- c$load_workbook(fname, "NIS_agent", "NIS_agent@1")
    output$worksheets<-renderText({paste("N worksheets: ",n)})
    r <- c$submit()
    num_errors<- 0
    if (length(r) > 0){
      issues <- data.frame(matrix(unlist(r), nrow=length(r), byrow=T, ncol = 5))
      colnames(issues)<-c("sheet","message","row","sheet_name","type")
      for(i in r){
        if (i["type"] == 3){
          num_errors <-num_errors + 1
        }
      }
      output$num_errors<-renderText({toString(num_errors)})
    }
    print("Returned from submit")
    if (num_errors == 0){
      r <- c$query_available_datasets()
      ds <- c$query_datasets(c(tuple("flow_graph_solution", "csv", "dataframe")))
      df <- py_to_r(ds[[1]][[3]])
      df$Value<-as.numeric(lapply(df$Value,str_replace ,pattern = ",",replacement = "."))
    }else{
      df<-issues
    }
    
    list(df1 = df, df2 = issues,  c= c)
  })
 
  
}




