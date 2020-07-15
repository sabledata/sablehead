---
title: "Length predictions from interorbital head measurements"
date: "`r format(Sys.time(), '%B %d, %Y')`"
output: 
  bookdown::html_document2:
    toc: true
    number_sections: false
    toc_depth: 3

---

<style>
p.caption {
  font-size: 0.9em;
  font-style: italic;
  color: grey;
  margin-right: 10%;
  margin-left: 10%;  
  text-align: justify;
}
</style>

```{r setup, include=FALSE, echo=FALSE}
  knitr::opts_chunk$set(echo = TRUE)
  options(knitr.table.format = "latex")
```

```{r global, echo=FALSE, warning=FALSE,message=FALSE}
library(RODBC)
library(knitr)
library(magick)
library(ggplot2)
library(dplyr)        # transform and summarize tabular data
library(xtable)       # produces tables
library(kableExtra)   # produces html tables with scrollbars, etc
library(bookdown)
library(ggfortify)

#  ----   G L O B A L --- F U N C T I O N S---------------------------------
  GetSQLData <- function(strSQL,strDbName) {    # connect to SQL Server
     cnn <- odbcDriverConnect(paste("Driver={SQL Server};Server=DFBCV9TWVASP001;", 
                                    "Database=",strDbName,";Trusted_Connection=Yes",sep=""))
     dat <- sqlQuery(cnn, strSQL)
     odbcClose(cnn)
    return(dat) 
    }
  
  panLab <- function( x, y, txt, ... ) {
    # Allows text to be placed at 0<x<1, 0<y<1).
    usr <- par( "usr" )
    par( usr=c(0,1,0,1) )
    text( x, y, txt, ... )
    par( usr=usr )
    return( NULL )
    }
  
  cleanf <- function(x){     # function to remove duplicates
    oldx <- c(FALSE, x[-1]==x[-length(x)])  
    # is the value equal to the previous    
    res <- x
    res[oldx] <- NA
    return(res)
    } 
  
  simpleCap <- function(x) {  # add capital first letter to each word
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
  }
  
  firstup <- function(x) {   # add capital first letter to first word
     substr(x, 1, 1) <- toupper(substr(x, 1, 1))
     x
  }
  
  format_cells <- function(df, rows ,cols, value = c("italics", "bold", "strikethrough")){

  # select the correct markup
  map <- setNames(c("*", "**", "~~"), c("italics", "bold", "strikethrough"))
  markup <- map[value]  

  for (r in rows){
    for(c in cols){

      # Make sure values are not factors
      df[[c]] <- as.character( df[[c]])
      # Update formatting
      df[r, c] <- paste0(markup, df[r, c], markup)
    }
  }

  return(df)
}
```

```{r, echo=FALSE}
  # for inline text where numbers are larger 
  inline_hook <- function(x) {
    if (is.numeric(x)) {
      format(x, digits = 2)
    } else x
  }
```

# Results

A simple linear regression was calculated to predict sablefish fork length based on interorbital head length.



```{r lm, include=FALSE, echo=FALSE}

    head        <- paste("select SPECIMEN_ID, sex, FORKLENGTHcm, Round_Weightg, ",
                   "InterOrbital_Distance from Head_Measurements  where InterOrbital_Distance is not null ",sep="")
    hd         <- GetSQLData(head,"Sablefish")      # read from sql server and store in csv
    # write.table(hd , file = paste("C:/DATA/ADMIN/StatsRefresher/Datasets/Reg/Mercury/","hd.csv",sep=''),
    #                row.names=FALSE, na="",col.names=TRUE, sep=",") 
    # hd   <- read.csv("C:/DATA/ADMIN/StatsRefresher/Datasets/Reg/Mercury/hd.csv")   # read in csv data
    
  # ---step002-- Scatter plot the raw data  # par(mfrow=c(1,2))
    prelimplothd <-ggplot(data=hd, aes(x=InterOrbital_Distance, y=FORKLENGTHcm), label=SPECIMEN_ID)+
    ggtitle('Fork Length vs. Interorbital')+
    ylab('Interorbital')+xlab('Fork Length')+
    geom_point()+
    geom_text(aes(label=SPECIMEN_ID),hjust=0, vjust=0)
    
    prelimplothd  

```



