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

W_cloud<-function(rdata,name,i)
{
  corpus1<-Corpus(VectorSource(rdata))
  #courpus1<-tm_map(corpus1,stemDocument)
  corpus1<- tm_map(corpus1, removePunctuation)
  corpus1 <- tm_map(corpus1, tolower)
  corpus1<- tm_map(corpus1, removeNumbers)
  
  corpus1 <- tm_map(corpus1, removeWords, c(stopwords("english"),"call","also","inform","yes","no","will","know", "date", "will","said","can","want","take","taken","worked","global","sep","different","work","responsible","skills","duration","end","need","years","till","office","experience","countires","using","bank","highlisghts","etc","if","summary","complete","new","india","mumbai","share","like","associate","senior","consultant","duration","problem","involved","details","computer","help","used","food","maharashtra","make","like","university"))
  #corpus1<-tm_map(corpus1,stripWhitespace)
  tdm<-TermDocumentMatrix(corpus1)
  m <- as.matrix(tdm)
  d <- data.frame(freq = sort(rowSums(m), decreasing = TRUE))
  # and put words to column, otherwise they would be lost when aggregating
  d$word <- row.names(d)
  # remove web address (very long string):
  d <- d[nchar(row.names(d)) < 12, ]
  # Stem words
  d$stem <- wordStem(row.names(d), language = "english")
  
  # aggregate freqeuncy by word stem and
  # keep first words..
  agg_freq <- aggregate(freq ~ stem, data = d, sum)
  agg_word <- aggregate(word ~ stem, data = d, function(x) x[1])
  d <- cbind(freq = agg_freq[, 2], agg_word)
  # sort by frequency
  d <- d[order(d$freq, decreasing = T), ]
  fname=paste(name,i,"_WC.png",sep="")
  png(fname, width=1280,height=800)
  #wordcloud(d, main="Title")
  # print wordcloud:
  #wordcloud(d$word, d$freq,min.freq=1,max.words=Inf, random.order=FALSE, rot.per=.15,colors=brewer.pal(8, "Dark2"),res=300)
  wordcloud(d$word, d$freq,scale=c(8,.9),min.freq=1,max.words=150, random.order=FALSE, rot.per=.15, use.r.layout=FALSE,colors=brewer.pal(8, "Dark2"))
  #wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=1,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
  dev.off()
}

sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 8)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    sentence <- tolower(sentence)
    corpus1<-Corpus(VectorSource(sentence))
    corpus1<-tm_map(corpus1,stripWhitespace)
    corpus1<- tm_map(corpus1, removePunctuation)
    corpus1<-tm_map(corpus1,removeNumbers)
    corpus1<-tm_map(corpus1,removeWords,c(stopwords("english")))
    corpus1<-tm_map(corpus1,stemDocument)
    
    #      initial_sentence <- sentence
    #      remove unnecessary characters and split up by word 
    #      sentence=gsub("\n"," ", sentence , fixed=TRUE)
    #      sentence <- gsub('[[:punct:]]', '', sentence)
    #      sentence <- gsub('[[:cntrl:]]', '', sentence)
    #      sentence <- gsub('\\d+', '', sentence)
    #      sentence <- tolower(sentence)
    wordList <- str_split(as.character(corpus1),'\\s+')
    words <- unlist(wordList)   
    words=words[(nchar(words[1:length(words)])<15)]
    #words=wordStem(words, language = "english")
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #words in each cataogry 
    vneg_w=paste(unique(words[!is.na(match(words, vNegTerms))]), collapse = ',')
    neg_w=paste(unique(words[!is.na(match(words, negTerms))]), collapse = ',')
    pos_w=paste(unique(words[!is.na(match(words, posTerms))]),collapse = ',')
    vpos_w=paste(unique(words[!is.na(match(words, vPosTerms))]),collapse = ',')
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vneg_w,neg_w, pos_w,vpos_w,vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    #newrow <- c(toString(words), score)
    newrow <- c(score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}    

main<-function()
{

    setwd("D:\\R\\Social Media")
    load_or_install(c("twitteR","Rfacebook","Rook","plyr","stringr","tm","wordcloud","Rstem","tm.plugin.tags","bitops","digest","ROAuth"))
  
    reqURL <- "https://api.twitter.com/oauth/request_token"

    accessURL <- "http://api.twitter.com/oauth/access_token"
    
    authURL <- "http://api.twitter.com/oauth/authorize"
    
    consumerKey <- "m7fyiITGYKKHMmkyiqUeyA"
    
    consumerSecret <- "QteVaXkW17oFFYcAcqyrgJFMVRrDD1Bc5Ahs8Xm4mfc"
    
    twitCred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=reqURL,accessURL=accessURL,authURL=authURL)
    
    download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
    
    twitCred$handshake(cainfo="cacert.pem")
    
    registerTwitterOAuth(twitCred)
    save(list="twitCred", file="twitteR_credentials")
    load("twitteR_credentials")
    registerTwitterOAuth(twitCred)
    rate.limit <- getCurRateLimitInfo( cainfo="cacert.pem")
    
    s <- searchTwitter('clubmahindra', n=1500,cainfo="cacert.pem")
    df = twListToDF(s)
    # df <- do.call("rbind", lapply(s, as.data.frame))
    # tweets_df = twListToDF(tweets)
    # tweets_df1 = twListToDF(s)
    
    # extract the usernames
    users <- unique(df$screenName)
    users <- sapply(users, as.character)
    # # make a data frame for the loop to work with 
    users.df <- data.frame(users = users, followers = "", stringsAsFactors = FALSE)
    
    for (i in 1:nrow(users.df)) 
    {
      # tell the loop to skip a user if their account is protected 
      # or some other error occurs  
      result <- try(getUser(users.df$users[i],cainfo="cacert.pem")$followersCount, silent = TRUE);
      if(class(result) == "try-error") next;
      users.df$followers[i] <- getUser(users.df$users[i],cainfo="cacert.pem")$followersCount
      # tell the loop to pause for 60 s between iterations to 
      # avoid exceeding the Twitter API request limit
      users.df$twts[i]=paste(df[grep(users.df$users[i],df$screenName),1],collapse=" ;")
      print('Sleeping for 60 seconds...')
      #Sys.sleep(60); 
    }
    
     write.table(twt_txt,file="tweets.csv",append = FALSE,eol="\n",sep=",",col.names=FALSE,row.names = FALSE)
      
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
      dat=cbind(df,sentimentScore(df[,1], vNegTerms, negTerms, posTerms, vPosTerms)[,1:8])
      
      b<-as.numeric((as.numeric(as.character(dat[,21]))*-5)+(as.numeric(as.character(dat[,22]))*-2)+(as.numeric(as.character(dat[,23]))*2)+(as.numeric(as.character(dat[,24]))*5))
      c=strptime(dat[,5],"%Y-%m-%d")
      dat=cbind(dat,b,c)
      colnames(dat)[17:26]=c("vneg_w","neg_w", "pos_w","vpos_w","vNegTerms","negTerms","posTerms", "vPosTerms","Sentiment_score","Day")
      dat=dat[order(dat[,25]),]
    
      write.table(dat,file="Socialmedia.csv",append=F,quote=T,sep=",",eol="\r\r\r",row.names=F,col.names=F)
    
    
    
    
    fb_oauth=fbOAuth(app_id="465667753537382", app_secret="cbb9e26d92a14dcb37dd3dd3e000f369", extended_permissions = TRUE)
    fb_oauth <- fbOAuth(app_id="123456789", app_secret="1A2B3C4D")
    save(fb_oauth, file="fb_oauth")
    load("fb_oauth")
    
    getTrends(period="weekly")
    searchTwitter("#orms")
    userTimeline("informs")
    
    # how many tweets of each drink
    nd = c(length(wine_txt), length(beer_txt), length(cofe_txt), length(soda_txt))
    
    # join texts
    drinks = c(wine_txt, beer_txt, cofe_txt, soda_txt) 
}