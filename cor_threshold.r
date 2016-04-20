
# cor_threshold <- function(time_window)
# {
	# time_window <- 20
	start_time <- Sys.time()
	require(plyr)
	require(cluster)
	memory.limit(size =  4000)   
	options(warn=-1)				# To Suppress Warnings
	
	y <- k <- NULL	
	msg <- count_diff <- difference <- NULL
	
	steps <- function(j, k, agg_ts, difference, count_diff, i , m) 					# Function for selective looping
	{	
		for(x in j:k)
		{
			y <- agg_ts[x, i]
			if(!is.na(y) && y > 0)
			{
				# print(sum(agg_ts[x:k, i]))
				difference[x, i] <<- difftime(agg_ts[x, 1], agg_ts[j, 1])			# Subtract time instances
				count_diff[x, i] <<- as.numeric(min(sum(agg_ts[x, i]), agg_ts[j, m]))	# Count of the minimum instance
				break;
			}
		}
	}
	
	agg_ts <- read.csv(choose.files(default = "./Output/Agg_Occ_min100_level0.csv", caption = "Input file" ), as.is = T, header = T, blank.lines.skip = TRUE, check.names = FALSE)     # Select the input file
	agg_ts$TIME_INS <- strptime(agg_ts$TIME_INS, format = "%Y-%m-%d %H:%M:%S")   
	# agg_ts <- read.csv(choose.files(default = "agg.csv", caption = "Input file"), as.is = T, header = T, blank.lines.skip = TRUE)     # Select the input file
	# agg_ts$TIME_INS <- strptime(agg_ts$TIME_INS, format = "%m/%d/%Y %H:%M")   
	time_threshold_down <- time_threshold_up <- cor_coeff <- total_count <- cl_count <- valid_count <- occurences <- 
	together <- event <- duplicate <- matrix(NA, nrow = ncol(agg_ts)-1, ncol = ncol(agg_ts)-1)
							# cl_count means cluster count, 
	kdata <- last_cl <- ncl <- cl <- group <- max_cl <- cl_dup_instances <- NULL
	flag <- 0

	mainDir <- getwd()								# Writing correlation groups to directory : Output/Correlations/
	subDir <- paste("Output/Threshold Correlations/", sep = "")
	cordir <- file.path(mainDir, subDir)
	dir.create(cordir, showWarnings = FALSE)
		
	print(ncol(agg_ts))
	for(m in 2:ncol(agg_ts))									# To validate results we need to run kmeans for every alert, current experiment is only for first alert
	{
		# m <- 4
		print(m)
		difference <- matrix(NA, nrow = nrow(agg_ts), ncol = ncol(agg_ts))				# Time Series of differences reinitialize
		colnames(difference) <- colnames(agg_ts)
		count_diff <- matrix(NA, nrow = nrow(agg_ts), ncol = ncol(agg_ts))				# Time Series of differences reinitialize
		colnames(count_diff) <- colnames(agg_ts)
		
		present <- which(agg_ts[, m] > 0)								# Get time values of presence of alert
		non_na <- NA
		for(i in 2:ncol(agg_ts))
		{
			# i <- 3
			print(i)			
			kdata <- NULL
			diff_count <- data.frame(NULL, NULL)							# 3 column structure for data
			invisible(mapply(steps, j = present, k = c(tail(present, -1), nrow(agg_ts)), MoreArgs = list(agg_ts, difference, count_diff, i , m)))	# Apply loop only between present vlaues
			non_na <- which(!is.na(count_diff[, i]))
			
			if(length(non_na) != 0)  
			{
				cols <- data.frame(TIME_DIFF = difference[non_na,i], COUNT = count_diff[non_na, i])
				diff_count <- rbind(diff_count, cols)			# Attach results of time differences				
			}
			non_na <- NA
			kdata <- rep(diff_count$TIME_DIFF, diff_count$COUNT)
			
			# ncl <- ceiling(sqrt(length(kdata)/2))				# Number of clusters **No specific reason for value though
			ncl <- 2
			if(length(kdata) < 2*ncl)
			{				
				next
			}
			cl <- pam(kdata, k = ncl)							# Apply clustering with ncl centers
			last_cl <- cl										# Backup the last cluster

			max_cl <- which(cl$clusinfo[,"size"] == max(cl$clusinfo[,"size"]))[[1]]		# Get number of the maximum size cluster
			group <- which(cl$cluster == max_cl)										# Extract elements of the cluster
			cl_dup_instances <- kdata[group]									# Backup maximum cluster

			if(max(data.frame(cl$silinfo["widths"])[, 3]) > 0)	#Apply clustering on the maximum cluster till there is overlapping of clusters.
		
			while(max(data.frame(cl$silinfo["widths"])[, 3]) != 1 )		# silinfo = 1 for overlapping clusters. For details type ?silhouette in R console .
			{
				last_cl <- cl	
				err_msg <- try(cl <- pam(cl_dup_instances, 2), silent = TRUE)			# Cluster with only 2 centers to remove outliers carefully 

				max_cl <- which(cl$clusinfo[,"size"] == max(cl$clusinfo[,"size"]))[[1]]
				group <- which(cl$cluster == max_cl)	
				cl_dup_instances <- cl_dup_instances[group]
				
				if(class(err_msg) == "try-error")				# When no more clusters can be formed break; Warning occured at this line and could not use suppressWarnings
				{
					flag <- 1 
					break
				}
			}
			
			cl <- last_cl		# Retrieve the last good non-overlapping cluster, with minimum number of outliers 
			
			max_cl <- which(cl$clusinfo[,"size"] == max(cl$clusinfo[,"size"]))[[1]]
			medoid <- cl$medoid[max_cl]
			group <- which(cl$cluster == max_cl)	
			
			std_deviation <- ceiling(sd(cl$data[group]))		# Obtain standard deviation of cluster
			cl_count[m-1, i-1] <- length(cl$data[group])
			
			time_threshold_down[m-1, i-1] <- medoid - std_deviation	# Set the threshold range within :- center plus minus std_deviation
			time_threshold_up[m-1, i-1] <- medoid + std_deviation
			
			if(medoid == 0)		# The outliers affect std_deviation to give negative values, hence the exception
			{
				time_threshold_down[m-1, i-1] <- 0 ; time_threshold_up[m-1, i-1] <- 0
			}
			
			together[m-1 ,i-1] <- length(kdata)
			valid_count[m-1, i-1] <- length(which(kdata <= time_threshold_up[m-1, i-1]))
			occurences[m-1, i-1] <- sum(agg_ts[present, m])
			
		}
	}
	
	cor_coeff <- valid_count/occurences			# Avoid divide by zero error 
	diag(cor_coeff) <- diag(valid_count) <- diag(occurences) <- 0
	invalid_count <- occurences - valid_count
	
	copy <- cor_coeff
	cor_coeff <- matrix(0, nrow = ncol(agg_ts)-1, ncol = ncol(agg_ts)-1)
	cor_coeff[which(copy > 0.75, arr.ind = TRUE)] <-  copy[which(copy > 0.75, arr.ind = TRUE)]
	
	temp <- occurences 
	occurences <- 0
	occurences <- matrix(0, nrow = ncol(agg_ts)-1, ncol = ncol(agg_ts)-1)
	occurences[which(copy > 0.75, arr.ind = TRUE)]	<- temp[which(copy > 0.75, arr.ind = TRUE)]

	alert_names <- tail(colnames(agg_ts), -1)
	rownames(occurences) <- colnames(occurences) <- alert_names
	rownames(valid_count) <- colnames(valid_count) <- alert_names
	rownames(cor_coeff) <- colnames(cor_coeff) <- alert_names

	# write.csv(time_threshold_up, file = "./Correlations_threshold_up.csv", row.names = TRUE)
	write.csv(occurences, file = "./Occurences.csv", row.names = TRUE)
	write.csv(valid_count, file = "./valid_count.csv", row.names = TRUE)	
	write.csv(together, file = "./Total together.csv", row.names = TRUE)
	write.csv(cor_coeff, file = "./Coefficient_correlation.csv", row.names = TRUE)
	
	end_time <- start_time - Sys.time()
options(warn=0)		# Reset the global option for warnings
}