library(tm)
library(openNLP)
library(tm.plugin.webmining)
library(plyr)
library(stringr)
library(NLP)
library(stringr)
library(tm.plugin.tags)
setwd("/home/ashish/tm_web/")

main<-function()
{
gsrc<- WebCorpus(GoogleNewsSource("US economy"))
lapply(inspect(gsrc), write, "US1.txt", append=TRUE, ncolumns=1000)
gjap<- WebCorpus(GoogleNewsSource("Japanese economy"))
lapply(inspect(gjap), write, "JAP1.txt", append=TRUE, ncolumns=1000)

n=tm_get_tags("Negativ")
p=tm_get_tags("Positiv")
afinn_list <- read.delim(file='AFINN/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)
#categorize words as very negative to very positive and add some movie-specific words
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1],n[is.na(match(n,afinn_list$word))])
posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1], p[is.na(match(p,afinn_list$word))])
vPosTerms <- afinn_list$word[afinn_list$score==5 | afinn_list$score==4]
dfU=web_df(gsrc,vNegTerms, negTerms, posTerms, vPosTerms)
US_df=aggregate(x = dfU[,8], by = list(dfU[,9]), FUN = "mean")
dfJ=web_df(gjap,vNegTerms, negTerms, posTerms, vPosTerms)
JPY_df=aggregate(x = dfJ[,8], by = list(dfJ[,9]), FUN = "mean")
Stock=read.csv("USD-JPY.csv",header=F, sep = "\t", quote = "\"")
colnames(Stock)=c("Date", "Last" ,'Open' ,'High' ,'Low ','Change')
Stock[,1]=as.Date(strptime(Stock[,1],"%b %d,%Y"))
x=JPY_df[(!is.na(match(JPY_df[,1],Stock[,1]))),2]
x1=US_df[(!is.na(match(US_df[,1],Stock[,1]))),2]
Stock<-Stock[ order(Stock[,1]),]
y=Stock[(!is.na(match(Stock[,1],JPY_df[,1]))),6]
y1=Stock[(!is.na(match(Stock[,1],US_df[,1]))),6]
y<-as.double(substr(y,1,4))
y1<-as.double(substr(y1,1,4))
z=(JPY_df[!is.na(match(JPY_df[,1],US_df[,1])),2]+US_df[!is.na(match(US_df[,1],JPY_df[,1])),2])/2
D=JPY_df[!is.na(match(JPY_df[,1],US_df[,1])),1]
w=data.frame(D,z)
colnames(w)=colnames(JPY_df)
w=rbind(w,JPY_df[is.na(match(JPY_df[,1],US_df[,1])),])
w=rbind(w,US_df[is.na(match(US_df[,1],JPY_df[,1])),])
y=Stock[(!is.na(match(Stock[,1],w[,1]))),6]
y<-as.double(substr(y,1,4))
x=w[(!is.na(match(w[,1],Stock[,1]))),2]
}


web_df<-function(gsrc,vNegTerms, negTerms, posTerms, vPosTerms)
{
DateTimeStamp=c("")
for(i in 1:length(gsrc))
{
DateTimeStamp[i]<-as.String(meta(gsrc[[i]], tag = "DateTimeStamp"))
#filteredDesc[i] =as.String(desc[grepl(stock,desc)])
}
desc=sapply(gsrc,FUN=function(x){attr(x,"Description") })
#filteredDesc[i] =as.String(desc[grepl(stock,desc)])
Heading=sapply(gsrc,FUN=function(x){attr(x,"Heading")})
df=data.frame(DateTimeStamp,Heading,desc)
Rdata <- data.frame(matrix(unlist(gsrc), nrow=length(gsrc), byrow=T))
df=cbind(as.data.frame(df),sentimentScore(Rdata[,1], vNegTerms, negTerms, posTerms, vPosTerms)[,2:5])
colnames(df)[4:7]=c('vNeg', 'neg', 'pos', 'vPos')
#colnames(Rdata)[9:12]=c('vNeg', 'neg', 'pos', 'vPos')
b<-as.numeric((as.numeric(as.character(df[,4]))*-5)+(as.numeric(as.character(df[,5]))*-2)+(as.numeric(as.character(df[,6]))*2)+(as.numeric(as.character(df[,7]))*5))
#c=as.numeric((as.numeric(df[,8])*-5)+(as.numeric(df[,9])*-2+(as.numeric(df[,10])*2+(as.numeric(df[,11])*5))))
#d=as.numeric((as.numeric(df[,12])*-5)+(as.numeric(df[,13])*-2+(as.numeric(df[,14])*2+(as.numeric(df[,15])*5))))
#df=cbind(df,b,c,d)
df=cbind(df,b)
#colnames(df)[16:18]=c("Sentiment_desc","Sentiment_text","Sentiment_header")
colnames(df)[8]=c("Sentiment_text")
df$Date<- as.Date( df$DateTimeStamp, '%Y-%m-%d')
return(df)
}







sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}    





