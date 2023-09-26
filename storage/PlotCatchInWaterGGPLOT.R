#  author:  L. Lacko

   # get required packages if not loaded
   if(!(require(rgdal))) install.packages("rgdal")
   if(!(require(maps))) install.packages("maps")
   if(!(require(ggplot2))) install.packages("ggplot2")
   if(!(require(ggspatial))) install.packages("ggspatial")


   require(rgdal)
   require(maps)
   require(ggplot2)
   library("ggspatial")
   library("RODBC")
 
   path<-"C:/github/sablehead/storage/"  # local  
   setwd(path)

   GetSQLData <- function(strSQL,strDbName) {    # load function SQL SERVER 
   cnn <- odbcDriverConnect(paste("Driver={SQL Server};Server=DFBCV9TWVASP001;",
          "Database=",strDbName,";Trusted_Connection=Yes",sep=""))
   dat <- sqlQuery(cnn, strSQL)
   odbcClose(cnn)
   return(dat)    }

   sql                     <-   "select * from Head_Measurements_Report where SLAT > 0"  # fishing event catch data
   pointdata       <-   GetSQLData(sql,"Sablefish")   # retrieve from seamount database
   #nrow(pointdata)   # count the number of rows

   lon      <- data.frame(pointdata$SLAT)
   lat       <- data.frame(pointdata$SLON)
   spid    <- data.frame(pointdata$SPECIMEN_ID)
   trip    <- data.frame(pointdata$TRIP_ID)
   df        <- as.data.frame(cbind(lon,lat,spid,trip))   # add year, lat, long, specimen_id to dataframe
                     
   canada                <-   readOGR(dsn = ".",layer ="awscntry_geo")  # open shapefile   
   shapefile_can   <-   fortify(canada)    # fortify for ggplot require.
   points_df           <-   fortify(df)
   head(df)

   map   <-     ggplot() +
                       geom_polygon(data = shapefile_can,  aes(x = long, y = lat, group = group), fill="gray40",  size = .2) +
                       geom_point(data =   points_df,   aes(pointdata.SLON, pointdata.SLAT,  fill=factor(pointdata.TRIP_ID),  size=0.7) , colour="gray50", shape=21, size=3) +            
                       theme_minimal()  + guides(fill=FALSE) + guides(size=FALSE) + guides(fill=guide_legend(title="Trip id")) +
                       theme(  plot.title = element_blank(),   axis.title.x = element_blank(),   axis.title.y = element_blank())

   map_projected  <- map +   # reproject to make it look better
   coord_map() +
   annotation_north_arrow(location = "bl", which_north = "true", 
                       pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                       style = north_arrow_fancy_orienteering) 

    map_projected  +
    annotation_scale() +
    coord_sf(crs = 4326)

   png("Figure1.png", width = 465, height = 225, units='mm', res = 400)  # export to png
   dev.off()                 
