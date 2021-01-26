

function <- {
       file_path_new <- file.path("C:/Users/alex.engel/Documents/GitHub/templogger")
       setwd(file_path_new)
       
       now <- Sys.time()
       file_name <- paste0(format(now, "%Y%m%d_%H%M%S_"), "data_set.csv"))
   write.csv(df, file_name)
   write.csv(data_frame, "file.csv") #for checking purposes
}

function <- {
   file_path_new <- file.path("C:/Users/alex.engel/Documents/GitHub/templogger")
   setwd(file_path_new)
   
   now <- Sys.time()
   file_name <- paste0(format(now, "%Y%m%d_%H%M%S_"), "MasterData.csv")
   write.csv(MasterData, file_name)
   # write.csv(data_frame, "file.csv") #for checking purposes
 
}
