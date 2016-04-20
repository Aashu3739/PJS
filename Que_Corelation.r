##Correlation using Queuing Approach
##Input : count_filtered_medusa.csv
##Output : ValidTable.csv, InvalidTable.csv, coeff_of_corr.csv

rm(list=ls(all=TRUE))

##Purpose: To write data frame to file
##Input : data frame, fileName
##Output(value returned) : None
write_to_file <- function(tbl, fname)
{
    write.csv(tbl, file=fname, row.names=F)    
}

##Purpose : To calculate coefficient of correlation  
##Input : distinct frame, valid_matrix, invalid_matrix
##Output(value returned) : coeff_corr
calc_coeff_corr <- function(distinct, valid_matrix, invalid_matrix)
{
    for(i in 1:length(distinct))                                                          ##for every distinct alert
    {
        for(j in 1:length(distinct))
        {
            if(i==j)					##for diagonal of matrix(alert i->i), skip
               next
            else
            {
                valid_val<-0
                invalid_val<-0
                valid_val<-valid_matrix[i, j]
                invalid_val <- invalid_matrix[i, j]
                if((valid_val + invalid_val) > 0 && (is.na(valid_val)==F) && (is.na(invalid_val)==F))             ##Handling NAs 
                { 
                    coeff_corr <- c(coeff_corr, valid_val/(valid_val+invalid_val))			##coeff=valid/(valid+invalid)
                }
                else
                {
                    coeff_corr<- c(coeff_corr, -1)
                }
           }    
       }
   }
   return(coeff_corr) 
}

##Global Variable declarations
que<-list()
invalid<-0
valid<-0
coeff_corr <-0
valid_val<- 0
invalid_val<-0
time_window<- as.numeric(5)
dttime_format <- "%m/%d/%Y %H:%M"  					##format string for date-time
valid_tbl_fname <- ".\\Output\\Que\\ValidTable.csv"                 				##file name string for valid table file                                                
invalid_tbl_fname <- ".\\Output\\Que\\InvalidTable.csv" 					##file name string for invalid table file 	
coeff_corr_fname <- ".\\Output\\Que\\coeff_of_corr.csv"					##file name string for coeff of correlation file 

log_data<-read.csv(file=".\\Input\\input_que.csv", as.is=T, header=F, skip=1, blank.lines.skip = TRUE)           ##select the input file
len_alert <- with(log_data, nchar(as.character(log_data[,2]))) 		               ##Counts number of characters in every alert
log_data<-log_data[as.logical(len_alert),]					##For alerts with length=0, as.logical converts it to False				
dist_index<-duplicated(log_data[,2])				                ##Finding distinct alerts
distinct<-log_data[!dist_index,2]

## declaring matrices and allocating dimensions based on number of distinct alerts
valid_matrix<-matrix(nrow = length(distinct),ncol = length(distinct))
dimnames(valid_matrix)<-list(1:length(distinct),1:length(distinct))
invalid_matrix<-matrix(nrow = length(distinct),ncol = length(distinct))
dimnames(invalid_matrix)<-list(1:length(distinct),1:length(distinct))

for(i in 1:length(distinct))					##For every distinct alert
{
    for(j in 1:length(distinct)-1)				                
    {		
        j<-j+1
        que<-0
        que<-que[-1]
        invalid<-0
        valid<-0
        for(k in 1:length(log_data[,2]))				##for each alert in log
        {
            if(log_data[k, 2] == distinct[i])				##title of alert pertains to alert i
            {				
                que<-rbind(que,log_data[k, 1])				##enqueue the alert timestamp
	next
             }
             else if(log_data[k, 2] == distinct[j])			 ##title of alert pertains to alert j
             {	
                 flag<-0	
	 while((length(que) > 0 ) && (flag != 1 ))
	 {						
	     x <- strptime(log_data[k, 1], dttime_format)
                     y <- strptime(que[1], dttime_format)			
	     if((difftime(x, y, units="mins")) <= time_window)	               ##assuming time difference set to time_window=5 units
	     {
	         valid <- valid + 1				                ##increment valid count by 1
	         flag <- 1
	     }
	     else
	     {											
	        invalid <- invalid + 1				##increment invalid count by 1
	     }
	     que<-que[-1]					##dequeue
	}
            }   
        }
        while(length(que)>0)
        {
            que<-que[-1]
            invalid <- invalid + 1				                ##increment invalid count by 1
        }
        valid_matrix[i,j]<-valid
        invalid_matrix[i,j]<-invalid
    }
}

for(k in 1:i)						##setting diagonal of matrix to 0
{
    invalid_matrix[k, k]<-0
}

##write valid table to file
write_to_file(valid_matrix, valid_tbl_fname)

##write invalid table to file
write_to_file(invalid_matrix, invalid_tbl_fname)
	
##calculating the coefficient of correlation and printing it for every alert pair i->j
coeff_corr <- calc_coeff_corr(distinct, valid_matrix, invalid_matrix)

##writing coefficient of correlation to file
write_to_file(coeff_corr, coeff_corr_fname)