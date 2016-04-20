##edg_ts filter
##Purpose : To keep only those alert log entries that have DOMINION = "edg_ts"
setwd("E:\\CART\\Cart_real_edg_ts")
log_format <- c("MSDATE","ID","NQA_ID","DOMINION","PLANE","ACTUAL_STATE","ACTUAL_NUMERIC","RISE_TYPE","TITLE","DESCRIPTION","OWNER","WORKFLOW_STATE","TIME_BEG","TIME_END","TIME_INS","TIME_UPD","XML","AFFECTED_ITEM_GRN","AFFECTED_ITEM_TYPE","AFFECTED_ASPECT","EXPECTED_STATE","EXPECTED_NUMERIC","SEVERITY","TICKET_NUMBER","HIST_TIME_INS")
fname <- "edg_ts_filtered_log.csv"
log_data<-read.csv(file.choose(), as.is=T, header=F, skip=1, blank.lines.skip = TRUE)           		##select the input file
new_log <- data.frame()
dominion_col_no <- as.numeric(4)
new_log = subset(log_data, log_data[,dominion_col_no] == "edg_ts",)    
colnames(new_log) <- log_format    
write.csv(new_log, file=fname, row.names=F)    
