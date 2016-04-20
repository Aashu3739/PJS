rm(list=ls(all=TRUE)) 
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

Cluster<-function(cl_no,Role)
{
  
    file_list <- list.files("C:/wamp/www/hr/livetable/idealprofiles/pdfresumes")
    if(length(file_list)> 0)
    {
      for (i in 1:length(file_list))
      {
           dest=paste("C:/wamp/www/hr/livetable/idealprofiles/pdfresumes",file_list[i],sep="/")
           exe <- "C:\\Program Files\\xpdfbin-win-3.03\\bin32\\pdftotext.exe"
           system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = T)
           
      }
      filetxt <- sub(".pdf", ".txt", file_list)
      fdest=paste("C:/wamp/www/hr/livetable/idealprofiles/sampleresumeuploads/",filetxt,sep="")
      fsrc=gsub("sampleresumeuploads","pdfresumes",fdest)
      file.rename(fsrc,fdest)
      do.call(file.remove,list(list.files("C:/wamp/www/hr/livetable/idealprofiles/pdfresumes",full.names=TRUE)))
    }
    w=as.character(unique(Role[,"Name"]))
    cl=list()
    dfk=data.frame(name=character(0),clname=character(0),Skill=character(0),keyword=character(0),kscore=integer(0))
    RI=data.frame(name=character(0),cl_no=integer(0),clname=character(0),score=integer(0))
    for(R in 1:length(w))
    {
            	   
           NameR=unique(Role[!is.na(match(Role[,"Name"],w[R])),"Name.of.Resume"])
           NameR <- sub(".pdf|.doc|.docx", ".txt", NameR)
           fname=paste(gsub("]","\\]",gsub("[","\\[",gsub(".","\\.",gsub(")","\\)",gsub("(","\\(",NameR,fixed=T),fixed=T),fixed=T),fixed=T),fixed=T),collapse="|")
           reuters=suppressWarnings(Corpus(DirSource(directory="C:/wamp/www/hr/livetable/idealprofiles/sampleresumeuploads",pattern=fname),readerControl = list(language = "en")))
           #reuters=reuters1
            ## Convert to Lower Case
            reuters <- tm_map(reuters, tolower)
            ## Remove Punctuations
            reuters <- tm_map(reuters, removePunctuation)
            ## Remove Numbers
            reuters <- tm_map(reuters, removeNumbers)
            ## Remove Stopwords
            reuters <- tm_map(reuters, removeWords, c(stopwords("english"),"\\f","call","also","inform","yes","no","will","know", "date", "will","said","can","want","take","taken","worked","global","sep","different","work","responsible","skills","duration","end","need","years","till","office","experience","countires","using","bank","highlisghts","etc","if","summary","complete","new","india","mumbai","share","like","associate","senior","consultant","duration","problem","involved","details","computer","help","used","food","maharashtra","make","like","university","yrs"))
            ## Eliminating Extra White Spaces
            reuters <- tm_map(reuters, stripWhitespace)
            ## create a term document matrix
            dtm <- DocumentTermMatrix(reuters)
            dtm1 <- as.matrix(dtm)
            #inspect(dtm[1:10, 11:21])
             name=c()
             clnumber=c()
             score=c()
             kw=unique(Role[!is.na(match(Role[,"Name"],w[R])),4:6])
             sum1=0
             B=sum(match(kw$Proficiency,"Begginer",nomatch=0))
             I=sum(match(kw$Proficiency,"Intermediate",nomatch=0))
             E=sum(match(kw$Proficiency,"Expert",nomatch=0))
             A <- matrix(data=c(B,I, E, 5, -3, 0, 0, 7, -5), nrow=3, ncol=3, byrow=TRUE)
             b <- matrix(data=c(100, 0, 0), nrow=3, ncol=1, byrow=FALSE)
             WT=as.integer(round(solve(A, b))) 
             for(k in 1:nrow(kw))
             {
               kw1=gsub("&","|",gsub("&&","|",gsub("and","|",gsub("/","|",gsub(", | ,","|",kw[k,3])))))
               for(j in 1:nrow(dtm1))
               {
                 name[j]=substr(rownames(dtm1)[j],1,nchar(rownames(dtm1)[j])-4)
                 clnumber[j]=R
                 
                 if(as.character(kw[k,2])=="Expert")
                 {
                   kscore=sum(dtm1[j,grep(tolower(kw1),colnames(dtm1))])
                   score[j]=kscore*WT[3]
                 }else if(as.character(kw[k,2])=="Intermediate")
                 {
                   kscore=sum(dtm1[j,grep(tolower(kw1),colnames(dtm1))])
                   score[j]=kscore*WT[2]
                 }else
                 {
                   kscore=sum(dtm1[j,grep(tolower(kw1),colnames(dtm1))])
                   score[j]=kscore*WT[1]
                 }
                 dfk=rbind(dfk,cbind(name[j],w[R],kw[k,1],kw[k,3],kscore))
               }
               
               sum1=sum1+score
             }
             RI=rbind(RI,cbind(name,R,w[R],sum1))
             ## do tfxidf
             dtm_tfxidf <- weightTfIdf(dtm) 
              m <- as.matrix(dtm_tfxidf)
              rownames(m) <- 1:nrow(m)
             cl[[R]]=list(rollapply(m, width=nrow(m), mean, by=nrow(m), by.colum=TRUE))
              ### don't forget to normalize the vectors so Euclidean makes sense
#               norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
#               m_norm <- norm_eucl(m)
#               
#               m_norm[is.nan(m_norm)]<-0.000
#               rollapply(matrix[,1], width=3, mean, by=3)          
#                   ### cluster into 10 clusters
                    
    }
                   
  return(list(cl,RI,dfk))

}

  
  rm(list = setdiff(ls(), lsf.str()))
  setwd("D:\\R\\Hr\\")
  load_or_install(c("RODBC","zoo","plyr","stringr","tm","wordcloud","Rstem","tm.plugin.tags","RColorBrewer","plyr"))
  kw = read.table("C:/wamp/www/hr/livetable/export/mycsv.csv",header=T,quote = "\"'",sep=",",stringsAsFactors=FALSE)
  k=Cluster(length(unique(kw[,3])),unique(kw[,c(3,6:10)]))
  save.image(file="Cluster.RData")
