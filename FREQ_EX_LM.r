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
      file_list <- list.files("C:/wamp/www/lm/myuploads/pdffiles")
       for (i in 1:length(file_list))
       {
            dest=paste("C:/wamp/www/lm/myuploads/pdffiles",file_list[i],sep="/")
            exe <- "C:\\Program Files\\xpdfbin-win-3.03\\bin32\\pdftotext.exe"
            system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = T)
            
       }
       mainDir <- "C:/wamp/www/lm/myuploads"
       subDir <- "txtfiles"
       dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
       filetxt <- sub(".pdf",".txt", file_list)
       fdest=paste("C:/wamp/www/lm/myuploads/txtfiles/",filetxt,sep="")
       fsrc=gsub("txtfiles","pdffiles",fdest)
       file.rename(fsrc,fdest)
       #do.call(file.remove,list(list.files("D:/R/lumiere/pdf",full.names=TRUE)))
  }

Freq_W<-function(sentences)
{
  sentence <- Corpus(VectorSource(sentences),readerControl = list(language = "en"))
  sentence <- tolower(sentence)
  corpus1<-Corpus(VectorSource(sentence))
  #corpus1<-tm_map(corpus1,stemDocument)
  corpus1<- tm_map(corpus1, removePunctuation)
  corpus1<-tm_map(corpus1,removeNumbers)
  corpus1<-tm_map(corpus1,removeWords,c(stopwords("english")))
  corpus1<-tm_map(corpus1,stripWhitespace)
  tdm<-TermDocumentMatrix(corpus1)
  m <- as.matrix(tdm)
  d <- data.frame(freq = sort(rowSums(m), decreasing = TRUE))
  # and put words to column, otherwise they would be lost when aggregating
  d$word <- row.names(d)
  # remove web address (very long string):
  d <- d[nchar(row.names(d)) < 15, ]
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


main<-function()
{
  rm(list = setdiff(ls(), lsf.str()))
  setwd("D:\\R\\lumiere")
  suppressWarnings(load_or_install(c("RODBC","gdata","tools","plyr","stringr","tm","Rstem","plyr")))
  file_list <- list.files("C:/wamp/www/lm/myuploads/excelfiles")
  df=data.frame()
  if(length(file_list)!=0)
  {
	  for(i in 1:length(file_list))
	  {
		file= read.xls(paste("C:/wamp/www/lm/myuploads/excelfiles/",file_list[i],sep=""), sheet = 1, header = TRUE,perl="C:\\strawberry\\perl\\bin\\perl.exe")
		len=nchar(file_ext(file_list[i]))
		fname=substr(file_list[i],1,nchar(file_list[i])-(len+1))
		df=rbind(df,cbind(fname,Freq_W(file[,1])))
		
	  }
	}
	  pdf_txt()
  file_list <- list.files("C:/wamp/www/lm/myuploads/txtfiles")
  for(i in 1:length(file_list))
  {
    file=suppressWarnings(readLines(paste("C:/wamp/www/lm/myuploads/txtfiles/",file_list[i],sep="")))
    fname=substr(file_list[i],1,nchar(file_list[i])-4)
    df=rbind(df,cbind(fname,Freq_W(file)))
    
  }
  #do.call(file.remove,list(list.files("C:/wamp/www/lm/myuploads/txtfiles",full.names=TRUE)))
  colnames(df)[1]=c("fname")
  channel <- odbcConnect("lumiere", uid="root")
  sqlDrop(channel,"lumiere.keywordf")
  sqlSave(channel,df, tablename = "lumiere.keywordf", append = FALSE,rownames =F)
  close(channel)
}
main()