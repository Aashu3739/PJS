library(e1071)
library(nnet)
library(randomForest)
library(quantmod)
library(tseries)
library(TFX)
library(RCassandra)

c=RC.connect(host = NULL, port = 9160L)
RC.use(c, "demo1")
r=RC.get.range.slices(c, "Users3",k.limit =550000)
df <- data.frame(matrix(unlist(r), nrow=550000, byrow=T))
df1<-df[,5:8]
colnames(df1)[1:4]=levels(unlist(df[1,1:4]))
stripday<-strptime(df1[,4],format="%Y%m%d")
fxdata<-data.frame(stripday,df1)
fxdata$TIME<-NULL
fxdata$TICKER<-NULL
fxdata$DATE<-NULL
colnames(fxdata)<-c("Date","High","Low","symbol","timestamp1")
df1<-fxdata[order(fxdata[,1],fxdata[,5]),]
df1$day.of.week <- format(df1$Date, "%A")
abc=df1

 High=aggregate(as.numeric(as.character(abc[,2])), list(abc[,1]), max)
Low=aggregate(as.numeric(as.character(abc[,3])), list(abc[,1]), min)
 Close= aggregate(as.numeric(as.character(abc[,2])), list(abc[,1]),tail, 1)
 Open=aggregate(as.numeric(as.character(abc[,2])), list(abc[,1]),head, 1)
rdata=cbind(Open,Low[,2],High[,2],Close[,2])
colnames(rdata)<-c("Date","Open","Low","High","Close")


rm(list= ls()[!(ls() %in% c('abc','df1'))])

#write data to .csv
write.table(rdata,"/home/ashish/usdjpy.csv",quote=FALSE,sep=",",row.names=FALSE)

EURUSD<-as.xts(read.zoo("/home/ashish/usdjpy.csv",sep=",",format="%Y-%m-%d",header=T))



RC.close(c)
