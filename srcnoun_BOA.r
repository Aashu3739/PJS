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

pdf_txt<-function()
{
  
  file_list <- list.files("D:/R/BOA/pdffiles")
  if(length(file_list)!=0)
  {   
    for (i in 1:length(file_list))
    {
      dest=paste("D:/R/BOA/pdffiles",file_list[i],sep="/")
      exe <- "C:\\Program Files\\xpdfbin-win-3.04\\bin64\\pdftotext.exe"
      system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = T)
      
    }
    mainDir <- "D:/R/BOA"
    subDir <- "txtfiles"
    dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
    filetxt <- sub(".pdf",".txt", file_list)
    fdest=paste("D:/R/BOA/txtfiles/",filetxt,sep="")
    fsrc=gsub("txtfiles","pdffiles",fdest)
    file.rename(fsrc,fdest)
    #do.call(file.remove,list(list.files("D:/R/lumiere/pdf",full.names=TRUE)))
  }
}

Freq_W<-function(sentences)
  
  sentence <- Corpus(VectorSource(sentences),readerControl = list(language = "en"))
  #   sentence <- tolower(sentence)
  corpus1<-Corpus(VectorSource(sentence))
  #corpus1<-tm_map(corpus1,stemDocument)
  #   corpus1<- tm_map(corpus1, removePunctuation)
  #   corpus1<-tm_map(corpus1,removeNumbers)
  corpus1<-tm_map(corpus1,removeWords,c(stopwords("english")))
  corpus1<-tm_map(corpus1,stripWhitespace)
  tdm<-TermDocumentMatrix(corpus1)
  m <- as.matrix(tdm)
  d <- data.frame(freq = sort(rowSums(m), decreasing = TRUE))
  # and put words to column, otherwise they would be lost when aggregating
  d$word <- row.names(d)
  # remove web address (very long string):
  d <- d[nchar(row.names(d)) < 50, ]
  d$stem <- wordStem(row.names(d), language = "english")
  # aggregate freqeuncy by word stem and
  # keep first words..
  agg_freq <- aggregate(freq ~ stem, data = d, sum)
  agg_word <- aggregate(word ~ stem, data = d, function(x) x[1])
  d <- cbind(freq = agg_freq[, 2], agg_word)
  # sort by frequency
  d <- d[order(d$freq, decreasing = T), ]
  return(d[,c(1,3)]) 
}  

TXTC<-function(text)
{
  text=tolower(text)
  text=gsub("[[:punct:]]|[[:cntrl:]]|[[:digit:]]", '',text)
  text=removeWords(text, c("\\f", stopwords(),'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','ll'))
  text=gsub("\n|\\s+"," ", text , fixed=TRUE)
  
}

main<-function()
{
  rm(list = setdiff(ls(), lsf.str()))
  setwd("D:/R/BOA")
  suppressWarnings(load_or_install(c("stringr","stringi","RODBC","gdata","tools","plyr","tm","Rstem","plyr")))
  
  pdf_txt()
  system('C:/Python27/python.exe Sry.py',wait = TRUE)
  df=data.frame() 
  file_list <- list.files("C:/wamp/www/lm/myuploads/excelfiles")
  
  if(length(file_list)!=0)
  {
    for(i in 1:length(file_list))
    {
      text= read.xls(paste("C:/wamp/www/lm/myuploads/excelfiles/",file_list[i],sep=""), sheet = 1, header = TRUE,perl="C:\\strawberry\\perl\\bin\\perl.exe")
      len=nchar(file_ext(file_list[i]))
      fname=substr(file_list[i],1,nchar(file_list[i])-(len+1))
      
      text<-keyword_replace(text,kw)
      df=rbind(df,cbind(fname,Freq_W(file[,1])))
      #write.table(text,file=paste0("C:/wamp/www/lm/myuploads/excelfiles/",fname,".xls"),append=FALSE)      
    }
  }
   
  file1_list <- list.files("D:/R/BOA/Noun")
  
  if(length(file1_list)!=0)
  {
    for(i in 1:length(file1_list))
    {
      
      text=suppressWarnings(readLines(paste("D:/R/BOA/Noun/",file1_list[i],sep="")))
      text=strsplit(text,",")
      datf=data.frame(matrix(unlist(text),byrow=T),stringsAsFactors=F)
      df1=count(tolower(datf[,1]))
      WORD=unique(sort(as.character(datf[,1])))
      FNAME=substr(file1_list[i],1,nchar(file1_list[i])-4)
      colnames(df1)[2]="FREQUENCY"
      rm(datf)
      datf=cbind(FNAME,WORD,df1[,2])
      colnames(datf)=colnames(df)
      #df=rbind(df,cbind(FNAME,WORD,df1[,2]))
      df=rbind(df,datf)
      rm(df1)
      colnames(df)=c("FNAME","WORD","FREQUENCY")
    }
  }
  
  channel <- odbcConnect("BOA", uid="root")
  tryCatch((sqlDrop(channel,"boa.keywordf")),error=function(e) e)
  sqlSave(channel,df, tablename = "boa.keywordf", append = FALSE,rownames =F)
  close(channel)
}
main()        