main<-function()
{

 library(deal) ## invoke DEAL

 ## read data 
 log_data<-read.csv(choose.files(), as.is = T, header = T, blank.lines.skip = TRUE)
 data<-log_data[2:ncol(log_data)]


  ## learn the initial network
  fit.nw <- network(data)

  ## make joint prior distribution
  fit.prior <- jointprior(fit.nw,12)


  fit <- getnetwork(learn(fit.nw,data,fit.prior))

  ## Do structural search and the model_string o network
  bmp("DAC.bmp")
  fit.search <- autosearch(fit,data,fit.prior,trace=TRUE)
  print(modelstring(getnetwork(fit.search)))
  dev.off()


  ## perturb ’thebest’ and rerun search twice.

  fit.heuristic <- heuristic(fit.search$nw,data,fit.prior,restart=2,degree=10,trace=TRUE,trylist=fit.search$trylist)
  thebest2 <- fit.heuristic$nw
  thebest2
 }

main()





