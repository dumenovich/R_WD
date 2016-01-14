
# lines to read
max.row<-8132
max.row.train<-700


get_text_val<-function (fname,n_row) {
  txt.original.val<-read_delim(fname, delim = "\t",quote = "")  
  
  # соединили вопрос с корректным ответом  
  #for (i in 1:n_row) {
    #cor_ans<-txt.original[i,paste("answer",txt.original$correctAnswer[i],sep="")]
    #txt.original$question[i]<-paste(txt.original$question[i],cor_ans,sep = " ")
  #}
  
  return(txt.original.val)
}


txt.original.val<-get_text_val("allen2/validation_set.tsv",max.row)

txt.new<-bind_rows(txt.original,txt.original.val)
