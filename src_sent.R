load("D:/R/SocialMedia/Naive Byess/Model.RData")
sentimentScore <- function(dataf,model){
  dataf[is.na(dataf)]="NONE"
  dat=dtm_idf(dataf)
  pred <- predict(model, dat, probability=TRUE)
  #   dataf1=cbind(dataf1,as.character(pred))
  return(as.character(pred))
}    