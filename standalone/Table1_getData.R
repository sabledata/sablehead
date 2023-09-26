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
   sd             <-    GetSQLData(details,"Sablefish")  # -- head details
   write.table( sd, file = paste(path,"head_measurements.csv", sep=''), row.names=FALSE, na="",col.names=TRUE,  sep=",")


    sample   <- paste("select  CASE WHEN TRIP_ID = 0 THEN 'Salmon' WHEN trip_id = 80378 THEN '2016 WCHG'  ",
                                       "ELSE '2016 WCVI ' END AS Survey, SUM(FL) AS FL, SUM(WR) AS WR, SUM(UJ) AS UJ, SUM(ED) AS ED,",
                                       "SUM(ID) AS ID, SUM(SL) AS SL, SUM(PP) AS PP, SUM(PH) AS PH, SUM(F) AS F, SUM(M) AS M, SUM(O) AS Otoliths,  ",
                                       "SUM(D) AS DNA, SUM(FL) AS Total  FROM    ",       
                                       " (SELECT   TRIP_ID, CASE WHEN Fork_Length > 0 THEN 1 ELSE 0 END AS FL, CASE WHEN Whole_Round_Weight > 0 THEN 1 ELSE 0 END AS WR, ",
                                       "CASE WHEN Upper_jaw_length > 0 THEN 1 ELSE 0 END AS UJ,  CASE WHEN Eye_Diameter > 0 THEN 1 ELSE 0 END AS ED, ",
                                       "CASE WHEN InterOrbital_distance > 0 THEN 1 ELSE 0 END AS ID, CASE WHEN Snout_length > 0 THEN 1 ELSE 0 END AS SL, ",
                                       " CASE WHEN Postorbital_Preoperculum > 0 THEN 1 ELSE 0 END AS PP, CASE WHEN Post_orbital_Head_length > 0 THEN 1 ELSE 0 END AS PH, ",
                                       " CASE WHEN SPECIMEN_SEX_DESC = 'FEMALE' THEN 1 ELSE 0 END AS F, CASE WHEN SPECIMEN_SEX_DESC = 'MALE' THEN 1 ELSE 0 END AS M, ",
                                       "CASE WHEN Otoliths_Collected = 'Y' THEN 1 ELSE 0 END AS O,  CASE WHEN DNA_Collected = 'Y' THEN 1 ELSE 0 END AS D  ",
                                       " FROM    dbo.Head_Measurements_Report) AS Samples ",
                                       "GROUP BY CASE WHEN TRIP_ID = 0 THEN 'Salmon' WHEN trip_id = 80378  ",
                                      " THEN '2016 WCHG' ELSE '2016 WCVI ' END", sep="")
   samplecnt           <-    GetSQLData(sample,"Sablefish")  # -- head details
   write.table(samplecnt , file = paste(path,"Table1.csv", sep=''), row.names=FALSE, na="",col.names=TRUE,  sep=",")

  