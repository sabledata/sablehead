#  source("c:/github/sablehead/standalone/getData.R")

if(!(require(RODBC))) install.packages("RODBC") 
path   <-  "c:/github/sablehead/standalone/"
yr        <-  2018

# ------------SQL SERVER LOAD-------------------------

GetSQLData <- function(strSQL,strDbName) { 
   require(RODBC)   
   cnn <- odbcDriverConnect(paste("Driver={SQL Server};Server=DFBCV9TWVASP001;",
        "Database=",strDbName,";Trusted_Connection=Yes",sep=""))
   dat <- sqlQuery(cnn, strSQL)
   odbcClose(cnn)
   return(dat) 
}

#index ------------------------------------------------------------------------------------------------------------------------------

   details     <-   paste("select * from Head_Measurements_Report", sep="")
   sd             <-    GetSQLData(details,"Sablefish")  # -- survey details
   write.table( sd, file = paste(path,"head_measurements.csv", sep=''), row.names=FALSE, na="",col.names=TRUE,  sep=",")

  