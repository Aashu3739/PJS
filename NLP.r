 library("openNLP")
 library("NLP")
 library("openNLPmodels.en")
 s <- paste(c(" Mayank Sharma , 61 years old , will join the board as a nonexecutive director Nov. 29 .
Mr . Deokant Kumar is chairman of Elsevier N.V. , the Dutch publishing group ."),
collapse = "")
s <- as.String(s)
sent_token_annotator <- Maxent_Sent_Token_Annotator() #senetence
word_token_annotator <- Maxent_Word_Token_Annotator()  #word
entity_annotator <-  Maxent_Entity_Annotator(language = "en", kind = c("person"), probs = FALSE,model ="C:\\apache-opennlp-1.5.3\\en-ner-person.bin")#path included of trained model
 entity_annotator
 a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
 s[entity_annotator(s, a2)]
 #annotate(s, entity_annotator, a2)
 #entity_annotator(s, a2)# entity name 
 
 
 
 pdf=paste(c(readLines(file.choose()), sep = "\n"),collapse=" ")

