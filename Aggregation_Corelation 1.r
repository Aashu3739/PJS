rm(list = ls(all = TRUE))
start_time <- Sys.time()
level <- 1800
min_count <-10
# Agg_method <- function(min_count, level)	     # min_count is the filter that removes alerts with less occurences and
                                                 
  						# level  is the aggregation time interval in seconds  

memory.limit(size =  4000)                      	
						 # Requesting 4000 Mbytes memory 

#log_data <- read.csv(file.choose(), as.is = T, header = T, blank.lines.skip = TRUE)     # Select the input file
	
log_data <- read.csv(file=".\\Input\\edg_ts_filtered_log.csv", as.is = T, header = T, blank.lines.skip = TRUE)     # Select the input file

log_data <- data.frame(TIME_INS = log_data$TIME_INS, TITLE = log_data$TITLE) 
log_data <- subset(log_data, log_data$TITLE != ",")
log_data <- subset(log_data, log_data$TIME_INS != "")
log_data <- subset(log_data, log_data$TITLE != "")

len_alert <- with(log_data, nchar(as.character(log_data$TITLE)))                                   # Counts number of characters in every alert
log_data <- log_data[as.logical(len_alert), ]      # For alerts with length = 0, as.logical converts it to False

filter_distinct <- NULL	                           # Distinct Alerts based on filter count 
level_ind <- NULL                                  # Aggregation level indices
time_sum <- 0

count <- NULL
count <- as.data.frame(table(log_data$TITLE))       # First column of count contains unique descriptions and second the number of occurences

filter_distinct <- as.character(subset(count[, 1], count[, 2] > min_count))	

temp <- strptime(log_data$TIME_INS, format = "%m/%d/%Y %H:%M")           # Converting string to timestamp ***Hard code
timediff_adj <- diff(temp)                          # timediff_adj contains time difference of adjacent rows Eg: if any = [1, 10, 3] then diff(any) = [9, -7]

for(i in 1:length(timediff_adj))                   # To seperate levels through entire log
{
   time_sum <- time_sum + timediff_adj[i]          # Count elapsed time over a level
   if(time_sum > level)                            # if time out 
   {
      time_sum <- 0                                # reset timer
      level_ind <- c(level_ind, i)                 # Note the level
   }
}
level_ind <- c(level_ind, nrow(log_data))          # level_ind has one element less(last level is not evaluated by diff) 
dist_level <- temp[level_ind]                      # Copying distinct time series

agg_matrix <- matrix(0, nrow = length(dist_level), ncol = length(filter_distinct))         # Aggregation matrix
colnames(agg_matrix) <- filter_distinct                                       # First column contains time series and remaining filtered alerts 

j <- 1
for(i in 1:length(log_data[, 1]))                                                              # Traversing log once
{
   str_ind <- as.character(log_data[i,2])
   result <- try(agg_matrix[j, str_ind] <- agg_matrix[j, str_ind] + 1, silent = TRUE)     # increment respective alert count, using string as index
   if(i  ==  level_ind[j])                         # If we reach one aggregation level 
   {
      j <- j + 1                                   # Consider next level
   }
}

agg_frame <- as.data.frame(agg_matrix)                                       # Matrix coerces all elements so using data frame
agg_frame <- cbind(TIME_INS = as.character(dist_level), agg_frame)        # Replace first column with time series


write.csv(agg_frame, file = ".\\Input\\Aggregated_Occurenecs.csv", row.names = FALSE)      # Writing to file


bin_matrix <- matrix(0, nrow = length(dist_level), ncol = length(filter_distinct))         # Aggregation matrix
colnames(bin_matrix) <- filter_distinct                                       # First column contains time series and remaining filtered alerts 

bin_matrix[,] <- as.logical(agg_matrix[,])
bin_frame <- as.data.frame(bin_matrix)                                       # Matrix coerces all elements so using data frame
bin_frame <- cbind(TIME_INS = as.character(dist_level), bin_frame)        # Replace first column with time series
write.csv(bin_matrix, file = ".\\Input\\Binary_Aggregated_Occurenecs.csv", row.names = FALSE)      # Writing to file

time_run <- start_time - Sys.time() 