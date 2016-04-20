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

WCluster<-function(clsum,a,ncl,Role,dfk)
{
  #     file_list <- list.files("C:/wamp/www/hr/resume/myuploads/pdfresumes/")
  #     for (i in 1:length(file_list))
  #     {
  #          dest=paste("C:/wamp/www/hr/resume/myuploads/pdfresumes",file_list[i],sep="/")
  #          exe <- "C:\\Program Files\\xpdfbin-win-3.03\\bin32\\pdftotext.exe"
  #          system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = T)
  #          
  #     }
  #     filetxt <- sub(".pdf", ".txt", file_list)
  #     fdest=paste("C:/wamp/www/hr/resume/myuploads/sampleresumeuploads/",filetxt,sep="")
  #     fsrc=gsub("sampleresumeuploads","pdfresumes",fdest)
  #     file.rename(fsrc,fdest)
  #     do.call(file.remove,list(list.files("C:/wamp/www/hr/resume/myuploads/pdfresumes",full.names=TRUE)))
  reuters=suppressWarnings(Corpus(DirSource("C:/wamp/www/hr/resume/myuploads/sampleresumeuploads/"),readerControl = list(language = "en")))
  ## Convert to Lower Case
  #do.call(file.remove,list(list.files("C:/wamp/www/hr/resume/myuploads/sampleresumeuploads",full.names=TRUE)))
  reuters <- tm_map(reuters, tolower)
  ## Remove Punctuations
  reuters <- tm_map(reuters, removePunctuation)
  ## Remove Numbers
  reuters <- tm_map(reuters, removeNumbers)
  ## Remove Stopwords
  reuters <- tm_map(reuters, removeWords, c(stopwords("english"),"call","also","inform","yes","no","will","know", "date", "will","said","can","want","take","taken","worked","global","sep","different","work","responsible","skills","duration","end","need","years","till","office","experience","countires","using","bank","highlisghts","etc","if","summary","complete","new","india","mumbai","share","like","associate","senior","consultant","duration","problem","involved","details","computer","help","used","food","maharashtra","make","like","university"))
  ## Eliminating Extra White Spaces
  reuters <- tm_map(reuters, stripWhitespace)
  
  ## create a term document matrix
  dtm <- DocumentTermMatrix(reuters)
  #inspect(dtm[1:10, 11:21])
  ## do tfxidf
  dtm_tfxidf <- weightTfIdf(dtm)
  ## do document clustering
  m <- as.matrix(dtm)
  #dfk=data.frame(name=character(0),Skill=character(0),keyword=character(0),kscore=integer(0))
  colnames(dfk)[1:5]=c("name","clname","Skill","keyword","kscore")
  colnames(clsum)[1:4]=c("name","clnumber","clname","sum1")
  df=data.frame(name=character(0),clnumbers=integer(0),clnames=character(0),distance=double(0))
  w=as.character(unique(Role[,"Name"]))
  for(i in 1:length(reuters))
  {
    V=c()
    #SUM=c()
    for(j in 1:ncl)
    {
      z=as.data.frame(a[[j]])
      f=matrix(0.00,nrow=1,ncol=ncol(z))
      colnames(f)=colnames(z)
      #SUM[j]=sum((colnames(f)%in%colnames(m)))
      f[1,colnames(z)[colnames(z)%in%colnames(m)]]=m[i,colnames(m)[colnames(m)%in%colnames(z)]]
      f[1,]=normalize.vector(f[1,])
      V[j]=mean(f[1,]-as.double(z[1,]))
    }
    fname=row.names(dtm)[i]
    name=substr(fname,1,nchar(fname)-4)
    distance=round((1-normalize.vector(V))*100)
    clnumberr=1:ncl
    df=rbind(df,cbind(name,clnumberr,w,distance))
    
    for(j in 1:ncl)
    {
      clnumber=j
      clname=w[j]
      kw=unique(Role[!is.na(match(Role[,"Name"],w[j])),2:4])
      sum1=0
      B=sum(match(kw$Proficiency,"Begginer",nomatch=0))
      I=sum(match(kw$Proficiency,"Intermediate",nomatch=0))
      E=sum(match(kw$Proficiency,"Expert",nomatch=0))
      A <- matrix(data=c(B,I, E, 5, -3, 0, 0, 7, -5), nrow=3, ncol=3, byrow=TRUE)
      b <- matrix(data=c(100, 0, 0), nrow=3, ncol=1, byrow=FALSE)
      WT=as.integer(round(solve(A, b))) 
      score=0
      for(k in 1:nrow(kw))
      {
        kw1=gsub("&","|",gsub("&&","|",gsub("and","|",gsub("/","|",gsub(", | ,","|",kw[k,3])))))
        
        name=substr(rownames(m)[i],1,nchar(rownames(m)[i])-4)
        if(as.character(kw[k,2])=="Expert")
        {
          kscore=sum(m[i,grep(tolower(kw1),colnames(m))])
          score=kscore*(WT[3])/E
        }else if(as.character(kw[k,2])=="Intermediate")
        {
          kscore=sum(m[i,grep(tolower(kw1),colnames(m))])
          score=kscore*WT[2]/I
        }else
        {
          kscore=sum(m[i,grep(tolower(kw1),colnames(m))])
          score=kscore*WT[1]/B
        }
        dfk=rbind(dfk,cbind(name,clname,Skill=kw[k,1],keyword=kw[k,3],kscore))
        sum1=sum1+score  
      }
      
      #clname=as.character(clsum[match(clnumber,clsum[,1]),4])
      clsum=rbind(clsum,cbind(name,clnumber,clname,sum1))
      #clnames=as.character(clsum[match(1:ncl,clsum[,1]),4])
      
    }
      
 }
    dfk[,"kscore"]=round(normalize.vector(as.numeric(as.character(dfk[,"kscore"])))*100)
    clsum[,"sum1"]=round(normalize.vector(as.numeric(as.character(clsum[,"sum1"])))*100)
    colnames(df)[1:4]=c("name","clnumber","clnames","distance")
    colnames(clsum)[4]=c("Score")
    return(list(clsum,df,dfk))
  }
  
  
  
  rm(list = setdiff(ls(), lsf.str()))
  setwd("D:\\R\\Hr\\")
  suppressWarnings(load_or_install(c("RODBC","RCurl","XML","plyr","ppls","stringr","tm","wordcloud","Rstem","tm.plugin.tags","RColorBrewer","plyr")))
  load("D:/R/Hr/Cluster.RData")
  datf=WCluster((k[[2]]),(k[[1]]),length(k[[1]]),(unique(kw[,c(3,8:10)])),k[[3]])
  channel <- odbcConnect("resume", uid="root")
  sqlDrop(channel, "resumeanalyser.Ranking")
  sqlDrop(channel, "resumeanalyser.Cldistance")
  sqlDrop(channel, "resumeanalyser.keywordf")
  sqlSave(channel, datf[[1]], tablename = "resumeanalyser.Ranking", append = FALSE)
  sqlSave(channel, datf[[2]], tablename = "resumeanalyser.Cldistance", append = FALSE)
  sqlSave(channel, datf[[3]], tablename = "resumeanalyser.keywordf", append = FALSE)
  #result  <-   paste("SELECT * FROM sakila.address;")
  #sqlQuery(channel, result,error=TRUE)
  close(channel)
  