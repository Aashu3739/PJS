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

callAPIInfo<- function(url, token){
  if (class(token)=="config"){
    url.data <- GET(url, config=token)
  }
  if (class(token)=="Token2.0"){
    url.data <- GET(url, config(token=token))
  }  
  if (class(token)=="character"){
    url <- paste0(url, "&access_token=", token)
    url.data <- GET(url)
  }
  if (class(token)!="character" & class(token)!="config" & class(token)!="Token2.0"){
    stop("Error in access token. See help for details.")
  }
  content <- fromJSON(rawToChar(url.data$content))
  if (length(content$error)>0){
    stop(content$error$message)
  }	
  return(content)
}


getPageInfo<- function(page,token){
  
  url <- paste0('https://graph.facebook.com/', page,sep="")
  # making query
  content <- callAPIInfo(url=url, token=token)
  ## retrying 3 times if error was found
  error <- 0
  while (length(content$error_code)>0){
    cat("Error!\n")
    Sys.sleep(0.5)
    error <- error + 1
    content <- callAPIInfo(url=url, token=token)		
    if (error==3){ stop(content$error_msg) }
  }
  #   if (length(content$data)==0){ 
  #     stop("No public posts mentioning the string were found")
  #   }
  df=as.data.frame(content)
  return(df)
}

sentimentScore <- function(dataf,model){
  dataf[is.na(dataf)]="NONE"
  dat=dtm_idf(dataf)
  pred <- predict(model, dat, probability=TRUE)
#   dataf1=cbind(dataf1,as.character(pred))
  return(as.character(pred))
 }    

writeintof<-function(page,dframe,ftype,dname)
{
  mainDir <- "D:/R/Social Media/facebook"
  subDir <- page
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
  fname=paste("D:/R/SocialMedia/facebook/",page,"/",ftype,".csv",sep="")
  write.table(dframe,fname,append=F,col.names=T,row.names=F,sep=",",quote = TRUE) 
}

facebook <-function(page1,fb_oauth,n=1000)
{
  fb_page <-suppressWarnings(getPage(page=page1, token=fb_oauth,n,feed=T))
  comments=data.frame()
  pflikes=data.frame()
  #
  for(i in 1:nrow(fb_page))
  {    
    possibleError <- tryCatch(
      post <- getPost(post=fb_page[i,7], n=1000, token=fb_oauth),error=function(e) e)
    if(inherits(possibleError, "error")) next  
    
    #Number of likes for a post and user info
    plikes=data.frame()
    if(!is.null((post[2])$likes))
    {
      for(l in 1:nrow((post[2])$likes))
      {     
        
        possibleError <- tryCatch(
          ( plikes=rbind(plikes,getUsers((post[2])$likes[l,2],token=fb_oauth,private_info=T))),error=function(e) e)
        if(inherits(possibleError, "error")) next
        
      }
      
      pflikes <-rbind(pflikes,suppressWarnings(cbind(fb_page[i,c(2,7)],plikes[,c(2:7)])))
      
    }
    
    #Number of comments for a post and user info
    if((fb_page[i,"comments_count"]>0)& (!is.null(post[3]$comments)))
    {
      cusers=data.frame()
      for(j in 1:nrow(post[3]$comments))
      {
        
        possibleError <- tryCatch(
          (cusers=rbind(cusers,getUsers((post[3]$comments)[j,1],token=fb_oauth,private_info=FALSE))),error=function(e) e)
        if(inherits(possibleError, "error")) next
        
      }
      comments=rbind(comments,suppressWarnings(cbind(fb_page[i,c(2,7)],(post[3]$comments)[,c(2:5)],cusers[,c(3:6)])))
      
    }
    print(i)
  }
  
  return(list(fb_page,pflikes,comments))
}

rm(list = setdiff(ls(), lsf.str())) 
setwd("D:/R/SocialMedia/facebook")
load_or_install(c("twitteR","Rfacebook","RODBC","Rcpp","Rook","plyr","stringr","tm","wordcloud","Rstem","tm.plugin.tags","bitops","digest","ROAuth"))

#   fb_oauth <-fbOAuth(app_id="716981824989164", app_secret="edb48344832e06bfb293c9e68a69191c",extended_permissions = TRUE)
#   save(fb_oauth, file="fb_oauth")

load("fb_oauth")


channel <- odbcConnect("sccard", uid="root")

Pinfo=sqlFetch(channel,"company_social_profile")
k=nrow(Pinfo)
pageid=as.character(Pinfo[k,"Fb_ID"])
pagename=as.character(Pinfo[k,"Fb_CompName"])
close(channel)

load("D:/R/SocialMedia/Naive Byess/Model.RData")
# n=tm_get_tags("Negativ")
# p=tm_get_tags("Positiv")
# afinn_list <- read.delim(file='AFINN/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
# names(afinn_list) <- c('word', 'score')
# afinn_list$word <- tolower(afinn_list$word)
# #categorize words as very negative to very positive and add some movie-specific words
# vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
# negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1],n[is.na(match(n,afinn_list$word))])
# posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1], p[is.na(match(p,afinn_list$word))])
# vPosTerms <- afinn_list$word[afinn_list$score==5 | afinn_list$score==4]



fb_pageinfo <- getPageInfo(page=pageid, token=fb_oauth)
fetchfb<- facebook(pageid,fb_oauth)
fbpage=fetchfb[[1]]
fbpage=cbind(fbpage,sentimentScore(fetchfb[[1]][,3],model))
# total_score=c()
# for(j in 1:nrow(fbpage))
# {
#   if((fbpage$likes_count[j]+fbpage$shares_count[j])!=0)
#   {
#     total_score[j]=as.integer(as.character(fbpage[j,11]))*(fbpage$likes_count[j]+fbpage$shares_count[j])
#   }
#   else
#   {
#     total_score[j]=as.integer(as.character(fbpage[j,11]))
#   }
# }
fbpage=cbind(fbpage,pagename)
colnames(fbpage)[c(11,12)]=c("sent_score","username")

pflikes=fetchfb[[2]]
comments=fetchfb[[3]]
comments=cbind(comments,sentimentScore(fetchfb[[3]][,4],model))  
# total_score=c()
# for(j in 1:nrow(comments))
# {
#   if((comments$likes_count[j])!=0)
#   {
#     total_score[j]=as.integer(as.character(comments[j,11]))*(comments$likes_count[j])
#   }
#   else
#   {
#     total_score[j]=as.integer(as.character(comments[j,11]))
#   }
# }

comments=cbind(comments,total_score)
colnames(comments)[11]=c("sent_score")
colnames(comments)[1]=c("PostName")
fbpage_info=fb_pageinfo[,grep("category|city|state|state|country|zip|phone|username|website|id|name|link|likes",colnames(fb_pageinfo))]

channel <- odbcConnect("sccard", uid="root")
#   sqlDrop(channel, "resumeanalyser.Ranking")
#     sqlDrop(channel, "resumeanalyser.Cldistance")
#     sqlDrop(channel, "resumeanalyser.keywordf")
sqlSave(channel,fbpage_info, tablename = "Score_Card.Basic_Info", append = T,rownames = FALSE)
# sqlSave(channel,comments, tablename = "Score_Card.Comments", append = T,rownames = FALSE)
# sqlSave(channel,pflikes, tablename = "Score_Card.Postlikes", append = T,rownames = FALSE)
# sqlSave(channel,fbpage, tablename = "Score_Card.Posts", append = T,rownames = FALSE)
close(channel)
#writeintof(pagename,fb_pageinfo,"fbpage_info")
writeintof(pagename,comments,"Comments")
writeintof(pagename,pflikes,"pflikes")
writeintof(pagename,fbpage,"Post")
