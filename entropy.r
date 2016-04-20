start_time <- Sys.time()

log_data<-read.csv(file.choose(), header=T, as.is=T)                                                  

inputdata<-data.frame(TS=log_data[, 15], TITLE=log_data[, 9])

inputdata = subset(inputdata, inputdata[,2] != "",)                                   ##Removing all blanks 
inputdata = subset(inputdata, inputdata[,1] != "",)     


dup<-duplicated(inputdata[, 2])		   	                                    ## Extracting the duplicate alerts on the basis of description 


distinct_alerts<-data.frame(TITLE=NULL)                                                   ## Creating the data frames for alerts




distinct_alerts<-data.frame(TITLE=inputdata[!dup, 2])	        ##first_occ has legal alerts based on description			

entropy<- matrix(0,nrow = length(distinct_alerts[, 1]), ncol = 17)    ##Creating a vector for storing the count of no of duplicate occurrence of alert. 


for(j in 1:length(distinct_alerts[,1]))			        ##For every distinct alert 
   {       	
      alert = subset( inputdata, inputdata[,2] == distinct_alerts[j,1])
           time <- alert[,1]
      time <-strptime(time, format = "%m-%d-%Y %H:%M")
      total_timestamp_count <- length(time)      
      
      Pi_monday <-  length(which( weekdays(time) == "Monday")) /  total_timestamp_count
      entropy[j, 1] <- Pi_monday * log(Pi_monday)*(-1)                                              ##entropy of monday 

      Pi_tuesday <-  length(which( weekdays(time) == "Tuesday")) /  total_timestamp_count
      entropy[j, 2]  <- Pi_tuesday * log(Pi_tuesday)*(-1) 
      
      Pi_wednesday <-  length(which( weekdays(time) == "Wednesday")) /  total_timestamp_count
      entropy[j, 3]  <- Pi_wednesday * log(Pi_wednesday)*(-1) 

     Pi_thursday <-  length(which( weekdays(time) == "Thursday")) /  total_timestamp_count
     entropy[j, 4]  <- Pi_thursday * log(Pi_thursday)*(-1) 

      Pi_friday <-  length(which( weekdays(time) == "Friday")) /  total_timestamp_count
      entropy[j, 5]  <- Pi_friday * log(Pi_friday)*(-1) 
 
      Pi_saturday <-  length(which( weekdays(time) == "Saturday")) /  total_timestamp_count
      entropy[j, 6] <- Pi_saturday * log(Pi_saturday)*(-1) 

      Pi_sunday <-  length(which( weekdays(time) == "Sunday")) /  total_timestamp_count
      entropy[j, 7] <- Pi_sunday * log(Pi_sunday)*(-1)    
      
       weekend_count <- length(which( weekdays(time) == "Saturday" | weekdays(time) == "Sunday" ) )
       weekday_count <- total_timestamp_count - weekend_count
       Pi_weekend <- weekend_count / total_timestamp_count 
       Pi_weekday <- weekday_count / total_timestamp_count
       entropy[j,8] <- Pi_weekday * log(Pi_weekday) *(-1)
       entropy[j,9] <- Pi_weekend * log(Pi_weekend) *(-1)
  
       

       hour<-data.frame(HOUR=(time)$hour) 
      
      Pi_0_4_hr <- nrow(subset( hour, (hour[,1] >= 0 & hour[,1] < 4)))  /  total_timestamp_count
      entropy[j,10] <- Pi_0_4_hr * log(Pi_0_4_hr) * (-1)  
      
      Pi_4_8_hr <- nrow(subset( hour, (hour[,1] >= 4 & hour[,1] < 8)))  /  total_timestamp_count
      entropy[j,11] <- Pi_4_8_hr * log(Pi_4_8_hr) * (-1)  

      Pi_8_12_hr <- nrow(subset( hour, (hour[,1] >= 8 & hour[,1] < 12)))  /  total_timestamp_count
      entropy[j,12] <- Pi_8_12_hr * log(Pi_8_12_hr) * (-1)  

      Pi_12_16_hr <- nrow(subset( hour, (hour[,1] >= 12 & hour[,1] < 16)))  /  total_timestamp_count
      entropy[j,13] <- Pi_12_16_hr * log(Pi_12_16_hr) * (-1)  

      Pi_16_20_hr <- nrow(subset( hour, (hour[,1] >= 16 & hour[,1] < 20)))  /  total_timestamp_count
      entropy[j,14] <- Pi_0_4_hr * log(Pi_0_4_hr) * (-1)  

      Pi_20_24_hr <- nrow(subset( hour, (hour[,1] >= 20 & hour[,1] < 24)))  /  total_timestamp_count
      entropy[j,15] <- Pi_20_24_hr * log(Pi_20_24_hr) * (-1)  
           
      Pi_working_hour <- nrow(subset( hour, (hour[,1] > 8 & hour[,1] < 17)))  /  total_timestamp_count   
      Pi_non_working_hour  <- nrow(subset( hour, (hour[,1] < 9 | hour[,1] > 16))) /  total_timestamp_count                           
      entropy[j,16] <- Pi_working_hour * log(Pi_working_hour) * (-1)  
      entropy[j,17] <- Pi_non_working_hour * log(Pi_non_working_hour) * (-1)
  
   }

entropy<-cbind(distinct_alerts,entropy[,1],entropy[,2],entropy[,3], entropy[,4], entropy[,5] ,entropy[,6] , entropy[,7], entropy[,8],entropy[,9],entropy[,10],entropy[,11],entropy[,12],entropy[,13],entropy[,14], entropy[,15], entropy[,16], entropy[,17])				 
                                   
colnames(entropy)<-c("Title","Mon","Tues","Wed", "Thurs","Fri","Sat","Sun","Weekdays","Weekends","0-4 Hr","4-8 Hrs","8-12 Hrs","12-16 Hrs","16-20 Hrs","20-24 Hrs","Work Hrs","Non Working Hrs") 


##Combining results to write in csv file 

write.csv(entropy,file ="entropy.csv" , row.names=FALSE )                          ##Creating a instance file to check legit and duplicate occurrences of all ditinct alerts

end_time <- Sys.time() - start_time  
print(end_time)

