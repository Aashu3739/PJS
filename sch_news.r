main<-function()
{
library(tm.plugin.webmining)
library(stringr)
library(tm)
library(zoo)
library(plyr)
mainDir <- "/home/ashish/tm_web"
D=Sys.Date()
D=substr(D,9,10)
subDir <- D
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir))
gsrc<- WebCorpus(GoogleNewsSource("US economy"))
gjap<- WebCorpus(GoogleNewsSource("Japanese economy"))
Wrintofile(gsrc,"US",D,"")
Wrintofile(gjap,"JAP",D,"J")



#cop <- Corpus(DirSource("/home/ashish/tm_web/23", encoding = "UTF-8"),readerControl = list(language = "lat"))
filename=paste("US",D,".txt",sep="")
lapply(inspect(gsrc), write, filename, append=TRUE, ncolumns=1000)
filename=paste("JAP",D,".txt",sep="")

lapply(inspect(gjap), write, filename, append=TRUE, ncolumns=1000)

}

Wrintofile<-function(gsrc,mname,D,header)
{
Heading=sapply(gsrc,FUN=function(x){attr(x,"Heading")})
P=as.character(as.integer(D))
filename=paste(mainDir,"/",P,"/",mname,".txt",sep="")
fname=paste("head",header,".txt",sep="")
if (file.exists(fname))
{
head1=read.table(fname,header = FALSE,sep="",blank.lines.skip=T)
k=unlist(head1)
Heading[is.na(match(Heading,k))]
write.table(Heading[is.na(match(Heading,k))],fname,append = T, quote = T, sep = " ",eol = "\n",row.names=F,col.names=F)
lapply(inspect(gsrc[is.na(match(Heading, k))]), write, filename, append=TRUE, ncolumns=1000)
}else
{
  write.table(Heading,fname,append = T, quote = T, sep = " ",eol = "\n",row.names=F,col.names=F)
  lapply(inspect(gsrc), write, filename, append=TRUE, ncolumns=1000)
}

}




