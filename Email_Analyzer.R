
is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])

load_or_install<-function(package_names)
{
  for(package_name in package_names)
  {
    if(!is_installed(package_name))
    {
       install.packages(package_name,repos="http://lib.stat.cmu.edu/R/CRAN")
    }
    library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)
  }
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
    match(c("regard","thanks","thank","regards"),words)
    words<-words[is.na(match(words,c("regard","thanks","thank","regards","Capgemini","kanbay")))]
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

main<-function()
{
  setwd("D:\\R\\email_Analysis")
  load_or_install(c("RODBC","plyr","stringr","tm","wordcloud","Rstem","tm.plugin.tags"))
  channel <- odbcConnect("dsn1", uid="root")
  #result  <-   paste("SELECT * FROM sakila.address;")
  #sqlQuery(channel, result,error=TRUE)
  dat = read.csv("email[001]", header = TRUE)
  
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
  dfU=web_df(dat[,13],vNegTerms, negTerms, posTerms, vPosTerms)
  dat=cbind(dat,sentimentScore(dat[,13], vNegTerms, negTerms, posTerms, vPosTerms)[,2:5])
  b<-as.numeric((as.numeric(as.character(dat[,16]))*-5)+(as.numeric(as.character(dat[,17]))*-2)+(as.numeric(as.character(dat[,18]))*2)+(as.numeric(as.character(dat[,19]))*5))
  c=strptime(dat[,2],"%Y-%m-%d")
  dat=cbind(dat,b,c)
  colnames(dat)[16:21]=c("vNegTerms","negTerms","posTerms", "vPosTerms","Sentiment_score","Day")
  dat=dat[order(dat[,21]),]
}
