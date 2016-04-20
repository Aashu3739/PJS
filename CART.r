##Classification and Regression Tree
#To prepare the rules and matrix for a specific cart

table_cor<-function(fit,log_data,z,o)
{

 nofnodes<-as.integer(nrow(fit$frame)/2) #total no of nodes

 noofpath<-c(1:nrow(fit$frame))          #no of possible split

 node_no<-rownames(fit$frame)			#extraxt node number
 
 path<-path.rpart(fit, node_no, pretty=0, print.it=FALSE)                #no of path at each node

 len1<-length(path)

 maxv<-order(-fit$frame[,"yval"]) #order in terms of maximum value of value
 
 
 leaf_wt<-matrix(0,nrow=1,ncol=(nrow(fit$frame)-nofnodes))  	#to choose max off (correlation coff* n)

 i<-1
 l<-1
 
 rl<-matrix(NA,nrow=(len1-nofnodes),ncol=ncol(log_data))
  
  maxwt<-0
  flag=-1


      while( (i<=nrow(fit$frame)) && (i<=len1)) 	#visit max of 7 split as maxdepth =2
	{
 	
 	    a<-matrix(NA,nrow=1,ncol=(ncol(log_data)-1))   	
		k<-maxv[i]												#start with max corr coff

   	{
	 if(length(path[[node_no[k]]])!=1)     #puting conditions should not be the "root" only 
  
  	  {
         
		spath<-path[[node_no[k]]]

		flag<-0
    
    		for (j in 2:length(spath))  	#extracting alert at condition and insert value 
  		{
     
           
    
    		if((as.integer(grepl(">=",spath[j])))!=0 && (fit$frame[k,"yval"]>=0.75)) 
    		  {
        		str<-strsplit(spath[j],split=">=")
        
        		m<-match(str[[1]][1], colnames(log_data)[2:ncol(log_data)])
				
				a[m]=fit$frame[k,"yval"] 
				
				rl[l,m]<-1
			   
			    flag=1
      			}
     		else
         	{
				if((as.integer(grepl(">",spath[j])))!=0 && (fit$frame[k,"yval"]>=0.75))
          		{
	
        		     str<-strsplit(spath[j],split=">=")
        
        	         m<-match(str[[1]][1], colnames(log_data)[2:ncol(log_data)])
					
					 a[m]=fit$frame[k,"yval"] 
					 
					 rl[l,m]<-1
			         
					 flag=1
     	
        		}
	     		else	
				{	
						if((as.integer(grepl("<=",spath[j])))!=0)
						{
	
							str<-strsplit(spath[j],split="<=")
        
							m<-match(str[[1]][1], colnames(log_data)[2:ncol(log_data)])
							
							a[m]=0
							rl[l,m]<-0
						}
						else if((as.integer(grepl("<",spath[j])))!=0)
						{
		
						str<-strsplit(spath[j],split="<")
        
						m<-match(str[[1]][1], colnames(log_data)[2:ncol(log_data)])
						
						a[m]=0
						rl[l,m]<-0
     
						}
				}
			}
	
        }

        if((j>2) && (fit$frame[k,"yval"]>=0.75) )		#putting conditing for rule 
		{
			leaf_wt[l]<-fit$frame[k,"wt"]*fit$frame[k,"yval"]
			#rl[l,1:(ncol(log_data)-1)]<-c(as.logical(a[(ncol(log_data)-1)]))
			rl[l,ncol(log_data)]<-fit$frame[k,"yval"]
			l<-l+1 
		}else
		{
	   
			if(flag==1 && (fit$frame[k,"yval"]>=0.75)) 	#for presence matrix 
			{			
			 
			  o$Pr_mt[(z-1),]<-a
			  
			}else										# for absence matrix
			{
			
			o$Ab_mt[(z-1),]<-a
			
			}

		}
	}
	
  }

 i<-i+1
 
}
  max_val<-order(-leaf_wt)
  if(l>1) 											#should not be NULL and always >0.75
  {
   o$Rule[(z-1),]<-rl[(max_val[1]),]
   }

 }

w_file<-function(MT,folder,f_name)					# writting to a file
{
   
   file_name<-paste(".\\Output\\Summ_aggr_op\\",folder,"\\",f_name,".csv", sep= "") 
   
   write.table(MT, file = file_name, sep = ",", col.names = NA,qmethod =  "double")
 }


#creating a tree 


main<-function()
{
 
 rm(list = ls(all = TRUE))
 library(rpart)
 library(R.oo)							# to pass the value by refernce by oops concept
 library(data.table) 
 
 #setwd("E:\\CART\\Cart_real_edg_ts")	#set to current directory 

 #log_data <- read.csv(file=".\\Input\\Binary_Aggregated_Occurenecs.csv",header=T,as.is=T)     #Select the input file
  log_data <- read.csv(choose.files(default = ".\\Input\\Binary_Aggregated_Occurenecs.csv", caption = "Input file"), as.is = T, header = T, blank.lines.skip = TRUE)     # Select the input file

  start_time <- Sys.time()
  
  f <- names(log_data) 				#header name
 
   o <- Object()  					#creating matrix which contains the matrix and rule
  
  o$Pr_mt<- array(NA, dim=c((ncol(log_data)-1),(ncol(log_data)-1)))			#presence matrix
  
  colnames(o$Pr_mt) <- c(colnames(log_data[2:ncol(log_data)])) 
 
  rownames(o$Pr_mt) <- c(colnames(log_data[2:ncol(log_data)])) 				

  o$Ab_mt<- array(NA, dim=c((ncol(log_data)-1),(ncol(log_data)-1)))			#absence matrix

  colnames(o$Ab_mt) <- c(colnames(log_data[2:ncol(log_data)])) 
  
  rownames(o$Ab_mt) <- c(colnames(log_data[2:ncol(log_data)]))   

  o$Rule<- array(NA, dim=c((ncol(log_data)-1),(ncol(log_data))))			# rules

  colnames(o$Rule) <- c(colnames(log_data[2:ncol(log_data)]),"val") 
 
  rownames(o$Rule) <- c(colnames(log_data[2:ncol(log_data)])) 
 			
 for(i in 2:ncol(log_data))				#formultae for diff. object
{
 	if(i==2)
	{
 	  form <- reformulate(paste(f[3:ncol(log_data)],collapse="+"),response=f[2])
	}else
	{if(i==ncol(log_data))
	  {
	
	   c<-paste(f[i], "~", sep=" ")
 	   form<-paste(c, paste(f[2:(i-1)], collapse = " + "), sep = "")
 	 
	  }else
	  {	
	   c<-paste(f[i], "~", sep=" ")
 	   d<-paste(c, paste(f[2:(i-1)], collapse = " + "), sep = " ")
 	   form<-paste(d, paste(f[(i+1):ncol(log_data)], collapse = " + "), sep = " + ")
	  }
         
	 }
     fit <- rpart(form,log_data,maxdepth=2)    # creating  a tree for mximum depth of 2
		
		if(nrow(fit$frame)>1)				#should be a tree ,not only root node  
		{	
		
		file_name<-paste(".\\Output\\Summ_aggr_op\\Binary_tree\\I",i,".bmp", sep= "_")
		bmp(filename=file_name)
		title1<-paste("Classification Tree for",f[i], sep= " ")
      	 plot(fit,compress=TRUE,branch=0,main=title1)		 				
		text(fit,xpd = NA, cex = 0.7,use.n=TRUE)
		dev.off()
		 table_cor(fit,log_data,i,o)
		}
 
	} #caling a function to write into a file 
     w_file(o$Rule,"Rules","RULE")
     w_file(o$Pr_mt,"presence_matrix","PRESENCE")
 	 w_file(o$Ab_mt,"absence_matrix","Absence")							
 
     end_time <- Sys.time()- start_time 
	print(paste("Execution time in seconds ",end_time))
 
 }	

 


main()

