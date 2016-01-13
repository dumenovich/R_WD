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

text_to_corpus<-function (txt.original,n_row) {
  corpus.original<- Corpus(VectorSource(txt.original$question[1:n_row]))
  
  return(corpus.original)
}

prepare_corpus<-function(corpus.original) {
  corpus.prepare<-tm_map(corpus.original,tolower)
  corpus.prepare<-tm_map(corpus.prepare,removePunctuation)
  corpus.prepare<-tm_map(corpus.prepare,removeNumbers)
  
  corpus.prepare<-tm_map(corpus.prepare,removeWords,stopwords("english"))
  corpus.prepare<-tm_map(corpus.prepare,stemDocument)
  
  corpus.prepare<-tm_map(corpus.prepare,stripWhitespace)

  corpus.prepare<-tm_map(corpus.prepare,PlainTextDocument)
  
  return(corpus.prepare)
}
  
get_matrix_terms<-function(corpus.prepare,n_row) {
  tdm.tmp<-TermDocumentMatrix(corpus.prepare)
  colnames(tdm.tmp)<-c(rep(1:n_row))
  
  m_tf.tmp<-t(as.matrix(tdm.tmp))
  
  return(m_tf.tmp)
}

get_matrix_tfidf<-function(m.t) {
  dims<-dim(m.t)
  n_docs<-dims[1]
  n_terms<-dims[2]
  
  # TF
  m.tf<-m.t/row_sums(m.t)  

  # TF norm
  #m.tf<-m.t
  #for (i in 1:n_docs) {
  #  a.tf<-1/sum(m.tf[i,]>0)
  #  m.tf[i,]<-0.5+0.5*m.tf[i,]/a.tf 
  #}
  
  # IDF  
  m.tfidf<-m.tf
  for (j in 1:n_terms) {
    n_TermInDocs<-sum(m.tfidf[,j]>0) # количество документов, в которых встречается j-терм
    m.tfidf[,j]<-m.tfidf[,j]*log(n_docs/n_TermInDocs) 
  }

  return(m.tfidf)
}




# lines to read
max_row<-800

txt_original<-get_text("allen/training_set.tsv",max_row)

# read file and get text variable
z_original<-text_to_corpus(txt_original,max_row)

# text manipulation
z_prepare<-prepare_corpus(z_original)

# Matrix Term Freq (TF)
m_t<-get_matrix_terms(z_prepare,max_row)
m_tfidf<-get_matrix_tfidf(m_t)

# Cluster
h<-hclust(dist(m_tfidf,method = "cosine"))
#h.num<-cutree(h,max_row*0.85)
h.num<-cutree(h,200)


t_c<-tbl_df(txt_original)
t_c<-slice(t_c,51:max_row)
t_c<-bind_cols(as.data.frame(h.num[51:max_row]),t_c)
t_c<-select(t_c,1:3)



# Create the document term matrix
dtMatrix <- create_matrix(t_c$question,language = "english",removePunctuation = T,removeNumbers = T,
                          removeStopwords = T, stemWords = T,stripWhitespace = T)

# Configure the training data
container <- create_container(dtMatrix, t_c$h.num, trainSize=1:700, testSize = 701:750, virgin=FALSE)

# train a SVM Model
#SVM <- train_model(container,"SVM") #, kernel="linear", cost=1)
#GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT",use_sgd = T)
#SLDA <- train_model(container,"SLDA")
#BOOSTING <- train_model(container,"BOOSTING")
#BAGGING <- train_model(container,"BAGGING")
#RF <- train_model(container,"RF")
#NNET <- train_model(container,"NNET")
#TREE <- train_model(container,"TREE")

#SVM_CLASSIFY <- classify_model(container, SVM)
#GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
#SLDA_CLASSIFY <- classify_model(container, SLDA)
#BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
#BAGGING_CLASSIFY <- classify_model(container, BAGGING)
#RF_CLASSIFY <- classify_model(container, RF)
#NNET_CLASSIFY <- classify_model(container, NNET)
#TREE_CLASSIFY <- classify_model(container, TREE)

analytics <- create_analytics(container,
                              cbind(MAXENT_CLASSIFY))


#analytics <- create_analytics(container,
#                              cbind(SVM_CLASSIFY, SLDA_CLASSIFY,
#                                    BOOSTING_CLASSIFY, BAGGING_CLASSIFY,
#                                    RF_CLASSIFY, GLMNET_CLASSIFY,
#                                    NNET_CLASSIFY, TREE_CLASSIFY,
#                                    MAXENT_CLASSIFY))
summary(analytics)

# CREATE THE data.frame SUMMARIES
topic_summary <- analytics@label_summary
alg_summary <- analytics@algorithm_summary
ens_summary <-analytics@ensemble_summary
doc_summary <- analytics@document_summary



