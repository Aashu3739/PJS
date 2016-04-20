install.packages("TFX")
library(TFX)
QueryTrueFX()
  yen <- ConnectTrueFX("USD/JPY,EUR/JPY,GBP/JPY,AUD/JPY,CAD/JPY,CHF/JPY", 
                     username="aashu", password="12345")
					 QueryTrueFX(yen)
					 QueryTrueFX(ConnectTrueFX("", "aashu", "12345", snapshot=TRUE))
					 
 fetchData <- function() {
    qtf <- QueryTrueFX()
    qtf$TimeStamp <- as.character(qtf$TimeStamp)
    names(qtf)[6] <- "TimeStamp (GMT)"
    qtf[, c(6, 1:3, 5:4)]
  }
  a=list()
repeat{ 
   fetchData()
   a<-rbind(a,fetchData()) 
   Sys.sleep(2)
   if(cnt==3)
   {
   break
   }   
cnt=cnt+1
   } 
