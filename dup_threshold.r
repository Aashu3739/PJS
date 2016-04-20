
dup_threshold <- function(min_count) # min_count to set the filter count
{
	require(cluster)
	options(warn=-1)		# Suppress warnings
	# min_count <- 30
	dup_count <- function(dup_instances, up, down, legit, duplicate, i)			# function for finding duplicate count
	{
		seq_dup <- diff(((dup_instances <= up) & (dup_instances >= down))*1)	# Obtain the sequence of duplicates
		legit[i] <<- as.numeric(table(seq_dup)["1"]) + !(seq_dup[min(which(seq_dup!=0))] == 1)		# column "1" contains count of breaks in sequence# Boundary check using !seq_dup[...]

		occ_count <- length(which(dup_instances >= down)) + length(which(dup_instances <= up)) - length(dup_instances) 	# Total values satisfying range 
		duplicate[i] <<- occ_count - legit[i]						# Duplicate count 
	}

	start_time <- Sys.time()
	# log_data <- read.csv(file = "./SockeyeRawData-Oct.csv", as.is = T, header = T, blank.lines.skip = TRUE)     # Select the input file
	log_data <- read.csv(choose.files(default = "SockeyeRawData-Oct.csv", caption = "Input file"), as.is = T, header = T, blank.lines.skip = TRUE)     # Select the input file

	log_data <- data.frame(TIME_INS = log_data$TIME_INS, TITLE = log_data$TITLE)
	len_alert <- with(log_data, nchar(as.character(log_data$TITLE)))		# Counts number of characters in every alert
	log_data <- log_data[as.logical(len_alert), ]      # For alerts with length = 0, as.logical converts it to False

	filter_distinct <- NULL	                           # Distinct Alerts based on filter count 
	count <- NULL
	count <- as.data.frame(table(log_data$TITLE))       # First column of count contains unique descriptions and second the number of occurences

	filter_distinct <- as.character(subset(count[, 1], count[, 2] > min_count))		# Filter based on count
	temp_time <- strptime(log_data$TIME_INS, format = "%m/%d/%Y %H:%M")				# Sockeye time format

	time_threshold_down <- time_threshold_up <-	total_count <- cl_count <- legit <-	duplicate  <- matrix(nrow = length(filter_distinct))
							# cl_count means cluster count, 
	dup_instances <- last_cl <- ncl <- cl <- group <- max_cl <- cl_dup_instances <- NULL
							# group will be used for obtaining the cluster group, cl_dup_instances for re-clustering
	flag <- 0

	for(i in 1:length(filter_distinct))#68)68)
	{
		flag <- 0
		present <- which(log_data$TITLE == filter_distinct[i])		# Get indices of presence of alert
		total_count[i] <- length(present)

		dup_instances <- diff(temp_time[present])					# Get time instances of presence of alert
		ncl <- ceiling(sqrt(length(dup_instances)/2))				# Number of clusters **No specific reason for value though
		cl <- pam(dup_instances, k = ncl)							# Apply clustering with ncl centers
		last_cl <- cl												# Backup the last cluster

		max_cl <- which(cl$clusinfo[,"size"] == max(cl$clusinfo[,"size"]))[[1]]		# Get number of the maximum size cluster
		group <- which(cl$cluster == max_cl)										# Extract elements of the cluster
		cl_dup_instances <- dup_instances[group]									# Backup maximum cluster

		# Apply clustering on the maximum cluster till there is overlapping of clusters.
		while(max(data.frame(cl$silinfo["widths"])[, 3]) != 1 )		# silinfo = 1 for overlapping clusters. For details type ?silhouette in R console .
		{
			last_cl <- cl	
			err_msg <- try(cl <- pam(cl_dup_instances, 2), silent = TRUE)			# Cluster with only 2 centers to remove outliers carefully 

			max_cl <- which(cl$clusinfo[,"size"] == max(cl$clusinfo[,"size"]))[[1]]
			group <- which(cl$cluster == max_cl)	
			cl_dup_instances <- cl_dup_instances[group]
			
			if(class(err_msg) == "try-error")				# When no more clusters can be formed break; Warning occured at this line and could not use suppressWarnings
			{
				flag <- 1 ;	break
			}
		}
		if(flag == 1)
		{
			time_threshold_down[i] <- NA ; time_threshold_up[i] <- NA ; next
		}
		cl <- last_cl		# Retrieve the last good non-overlapping cluster, with minimum number of outliers 
		
		max_cl <- which(cl$clusinfo[,"size"] == max(cl$clusinfo[,"size"]))[[1]]
		medoid <- cl$medoid[max_cl]
		group <- which(cl$cluster == max_cl)	
		
		std_deviation <- ceiling(sd(cl$data[group]))		# Obtain standard deviation of cluster
		cl_count[i] <- length(cl$data[group])
		
		time_threshold_down[i] <- medoid - std_deviation	# Set the threshold range within :- center plus minus std_deviation
		time_threshold_up[i] <- medoid + std_deviation
		
		if(medoid == 0)		# The outliers affect std_deviation to give negative values, hence the exception
		{
			time_threshold_down[i] <- 0 ; time_threshold_up[i] <- 0
		}
		
		dup_count(dup_instances, time_threshold_up[i], time_threshold_down[i], legit, duplicate, i) # Function to find legit and duplicate count
	}

	threshold <- cbind(filter_distinct, time_threshold_down, time_threshold_up, total_count, cl_count, cl_count - legit - duplicate, legit, duplicate)
	colnames(threshold) = c("Title", "Upper_limit", "Lower_limit", "Total_count", "Cluster_count", "Outliers", "Legit_count", "Duplicate_count")
	write.csv(threshold, file = "./Duplicate_threshold.csv", row.names = FALSE)
	end_time <- start_time - Sys.time()
	options(warn=0)		# Reset the option for warnings
	print(end_time)
}