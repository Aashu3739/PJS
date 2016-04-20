###############################


###############################
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


main<-function()
{
  
  setwd("D:\\R\\lumiere")
  suppressWarnings(load_or_install(c("RODBC","plyr","tools","stringr","gdata","tm","wordcloud","Rstem","tm.plugin.tags","RColorBrewer","plyr")))
  channel <- odbcConnect("lumiere", uid="root")
  W=sqlFetch(channel, "word", rownames =T) # get the lot
  fname=as.character(W[,3])
  words=as.character(W[1,1])
  lengthw=W[1,2]
  file_list <- list.files("C:/wamp/www/lm/myuploads/excelfiles")
  df=data.frame()
  if(length(file_list)>0)
  {
      for(i in 1:length(file_list))
      {       
        len=nchar(file_ext(file_list[i]))
        fname1=substr(file_list[i],1,nchar(file_list[i])-(len+1))
        if(!is.na(match(fname1,fname)))
          {    
              file= read.xls(paste("C:/wamp/www/lm/myuploads/excelfiles/",file_list[i],sep=""), sheet = 1, header = TRUE,perl="C:\\strawberry\\perl\\bin\\perl.exe")
              Line=file[grep("development",tolower(file[,3])),3]
              for(j in 1:length(Line))
              {
                    V=strsplit(as.character(Line[[1]]),words)
                    if(length(v[[1]])>2)
                    {
                      df=rbind(df,cbind(fname1,words,Line[j]))
                    }
                    else
                    {
                        if((nchar(V[[1]][1])-lengthw)>0)
                        {
                          Str1=substr(V[[1]][1],nchar(V[[1]][1])-lengthw,nchar(V[[1]][1]))
                        }
                        else{
                          
                          str1=substr(V[[1]][1],1,nchar(V[[1]][1]))
                            }
                        if((nchar(V[[1]][2])-lengthw)>0)
                        {
                          Str2=substr(V[[1]][2],1,lengthw)
                        }
                        else{
                          
                          str2=substr(V[[1]][2],1,nchar(V[[1]][2]))
                        }
                        df=rbind(df,cbind(fname1,words,paste(str1,str2,sep=words)))
                    }
                }
              
           }
        }
  }    
    
  
  file_list <- list.files("C:/wamp/www/lm/myuploads/stext")
  if(length(file_list)>0)
  {
        for(i in 1:length(file_list))
        {
          len=nchar(file_ext(file_list[i]))
          fname1=substr(file_list[i],1,nchar(file_list[i])-(len+1))
          if(!is.na(match(fname1,fname)))
          {     
                file=suppressWarnings(readLines(paste("C:/wamp/www/lm/myuploads/stext/",file_list[i],sep="")))
                Line=file[grep(words,tolower(file))]
                for(j in 1:length(Line))
                {
                      V=strsplit(as.character(Line[[j]]),words)
                      if(length(V[[1]])>2)
                      {
                        df=rbind(df,cbind(fname1,words,Line[j]))
                      }
                      else
                      {
                          if((nchar(V[[1]][1])-lengthw)>0)
                          {
                            Str1=substr(V[[1]][1],nchar(V[[1]][1])-lengthw,nchar(V[[1]][1]))
                          }
                          else{
                            
                            Str1=substr(V[[1]][1],1,nchar(V[[1]][1]))
                          }
                          if((nchar(V[[1]][2])-lengthw)>0)
                          {
                            Str2=substr(V[[1]][2],1,lengthw)
                          }
                          else{
                            
                            Str2=substr(V[[1]][2],1,nchar(V[[1]][2]))
                          }
                          df=rbind(df,cbind(fname1,words,paste(Str1,Str2,sep=words)))
                      }
                }
        
           }
        }
        
  }
  colnames(df)[c(1,3)]=c("fname","txt")
  
  #sqlDrop(channel,"lumiere.search")
  suppressWarnings(sqlSave(channel,df, tablename = "lumiere.search", append = T,rownames =F))
  close(channel)
}

rm(list = setdiff(ls(), lsf.str()))
args <- commandArgs(TRUE)
main()