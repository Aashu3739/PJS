require(plyr)
require(stringr)
library(tm)
library(wordcloud)
library(Rstem)

chron(dt[1],format=c(dates="%B %d,%Y", times="h:m"))


#load up word polarity list and format it
afinn_list <- read.delim(file='AFINN/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)

txt=scan("Japanese.txt", what="character", sep=NULL)
txt <- tolower(txt)
txt <- removeWords(txt, c("\\f", stopwords()))

corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, removePunctuation)
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
d <- data.frame(freq = sort(rowSums(m), decreasing = TRUE))

# Stem words
d$stem <- wordStem(row.names(d), language = "english")

# and put words to column, otherwise they would be lost when aggregating
d$word <- row.names(d)

# remove web address (very long string):
d <- d[nchar(row.names(d)) < 20, ]

# aggregate freqeuncy by word stem and
# keep first words..
agg_freq <- aggregate(freq ~ stem, data = d, sum)
agg_word <- aggregate(word ~ stem, data = d, function(x) x[1])

d <- cbind(freq = agg_freq[, 2], agg_word)

# sort by frequency
d <- d[order(d$freq, decreasing = T), ]

# print wordcloud:
wordcloud(d$word, d$freq)
 
#categorize words as very negative to very positive and add some movie-specific words
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1], "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")
posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1], "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious")
vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", "fascinating", "dazzling", "legendary")

#function to calculate number of words in each category within a sentence
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
 
 Rdata=cbind(as.data.frame(Rdata),sentimentScore(Rdata[,8], vNegTerms, negTerms, posTerms, vPosTerms)[,2:5])
 
#build tables of positive and negative sentences with scores
posResult <- as.data.frame(sentimentScore(posText, vNegTerms, negTerms, posTerms, vPosTerms))
negResult <- as.data.frame(sentimentScore(negText, vNegTerms, negTerms, posTerms, vPosTerms))
posResult <- cbind(posResult, 'positive')
colnames(posResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')
negResult <- cbind(negResult, 'negative')
colnames(negResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')    
 
#combine the positive and negative tables
results <- rbind(posResult, negResult)





colnames(Rdata)[9:12]=c('vNeg', 'neg', 'pos', 'vPos')
b<-as.numeric((as.numeric(Rdata[1:25,9])*-5)+(as.numeric(Rdata[1:25,10])*-2+(as.numeric(Rdata[1:25,11])*2+(as.numeric(Rdata[1:25,12])*5))))
