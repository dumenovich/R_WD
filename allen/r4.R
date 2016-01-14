library(readr)
library(tm)
library(SnowballC)
library(slam)
library(proxy)
library(RTextTools)
library(dplyr)


get_text<-function (fname,n_row) {
  txt.original<-read_delim(fname, delim = "\t",quote = "")  

  # соединили вопрос с корректным ответом  
  for (i in 1:n_row) {
    cor_ans<-txt.original[i,paste("answer",txt.original$correctAnswer[i],sep="")]
    txt.original$question[i]<-paste(txt.original$question[i],cor_ans,sep = " ")
  }

  return(txt.original)
}

get_text_val<-function (fname,n_row) {
  txt.original.val<-read_delim(fname, delim = "\t",quote = "")  
  
  return(txt.original.val)
}




text_to_corpus<-function (txt.original,n_row) {
  corpus.original<- Corpus(VectorSource(txt.original$question[1:n_row]))

  return(corpus.original)
}

prepare_corpus<-function(corpus.original) {
  
  my_stopwords<-c("best","following","likely","can",  # row_sums>150
                  "one","scientists","will",          # row_sums>100
                  "student","students","scientist","within","across","like","may","without","outer","per","usually")  # row_sums>10
  
  
  corpus.prepare<-tm_map(corpus.original,tolower)
  corpus.prepare<-tm_map(corpus.prepare,removePunctuation)
  corpus.prepare<-tm_map(corpus.prepare,removeNumbers)
  
  corpus.prepare<-tm_map(corpus.prepare,removeWords,stopwords("english"))
  corpus.prepare<-tm_map(corpus.prepare,removeWords,my_stopwords)
  corpus.prepare<-tm_map(corpus.prepare,stemDocument)
  
  corpus.prepare<-tm_map(corpus.prepare,stripWhitespace)

  corpus.prepare<-tm_map(corpus.prepare,PlainTextDocument)
  
  return(corpus.prepare)
}
  
get_matrix_terms<-function(corpus.prepare,n_row) {
  tdm.tmp<-TermDocumentMatrix(corpus.prepare,control = list(weighting = function(x) weightTfIdf(x, normalize = F)))
  #tdm.tmp<-TermDocumentMatrix(corpus.prepare,control = list(weighting = function(x) weightSMART(x, spec = "Ltn")))
  colnames(tdm.tmp)<-c(rep(1:n_row))
  
  return(tdm.tmp)
}



# lines to read
max.row<-2500
max.row.val<-0#8132
max.row.train<-700
#max.row.train<-2500
#max.row.train<-10632

txt.original<-get_text("allen/training_set.tsv",max.row)
txt.original.val<-get_text_val("allen2/validation_set.tsv",max.row.val)

txt.original<-bind_rows(txt.original,txt.original.val)



# read file and get text variable
corp.original<-text_to_corpus(txt.original,max.row.train)
# text manipulation
corp.prepare<-prepare_corpus(corp.original)

# Matrix Term Freq (TF)
tdm<-get_matrix_terms(corp.prepare,max.row.train)
m.tfidf<-t(as.matrix(tdm))

#zz<-col_sums(m.tf>0)
#z3<-zz

##m_t<-get_matrix_terms(z_prepare,max_row)
##m_tfidf<-m_t
#m_tfidf<-get_matrix_tfidf(m_t)

# Cluster
h<-hclust(dist(m.tfidf,method = "cosine"))
h.num<-cutree(h,250)


h.num.2<-c(h.num,rep(NA,max.row+max.row.val-max.row.train))
#h.num.2<-h.num

t_c<-tbl_df(txt.original)
t_c<-bind_cols(as.data.frame(h.num.2),t_c)


# Create the document term matrix
dtMatrix <- create_matrix(t_c$question,language = "english",removeNumbers = T,removePunctuation = T,
                          removeStopwords = T,toLower = T,stripWhitespace = T,weighting = weightTfIdf)

# Configure the training data
container <- create_container(dtMatrix, t_c$h.num.2, trainSize=1:700, testSize = 701:2500, virgin=FALSE)


MAXENT <- train_model(container,"MAXENT",use_sgd = T)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)

# CREATE SUMMARIES
analytics <- create_analytics(container, MAXENT_CLASSIFY)
#summary(analytics)
doc_summary <- analytics@document_summary


#build matrix
summ_num<-c(h.num,doc_summary$MAXENTROPY_LABEL)

t_c_2<-select(t_c,c(2,3,4,5,6,7,8))
t_c_2<-bind_cols(as.data.frame(summ_num),t_c_2)







