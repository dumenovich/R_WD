
m_qa<-t_c_2

corAnswer<-NULL

#m_qa_filter<-filter(m_qa,m_qa$id<102500)

for (j in 1:2500) {

n_q<-j
n_id<-m_qa$id[n_q]

n_clust<-m_qa$summ_num[n_q]
m_qa_filter<-filter(m_qa,(m_qa$summ_num==n_clust) & (m_qa$id<102500))
  
sum_cora<-""
for (i in 1:dim(m_qa_filter)[1]) {
  #if (!is.na(m_qa_filter$correctAnswer[i])) {
  if (m_qa_filter$id[i]!=n_id) {
    cor_ans<-m_qa_filter[i,paste("answer",m_qa_filter$correctAnswer[i],sep="")]
    #sum_cora<-paste(sum_cora,m_qa_filter$question[i],sep = " ")
    sum_cora<-paste(sum_cora,cor_ans,sep = " ")
  }
}

  
# build text variable
corp.train<-data.frame(id=1:5,
                       question=c(sum_cora,m_qa$answerA[n_q],m_qa$answerB[n_q],m_qa$answerC[n_q],m_qa$answerD[n_q]),
                       corA=c("sumA","A","B","C","D"),
                       stringsAsFactors = F)
# text manipulation
corp.train.corp<-text_to_corpus(corp.train,5)
corp.train.prepare<-prepare_corpus(corp.train.corp)

tdm.train<-get_matrix_terms(corp.train.prepare,5)
m.tfidf.train<-t(as.matrix(tdm.train))

dist.train<-dist(m.tfidf.train,method = "cosine")

nn<-1
if (dist.train[2]<dist.train[nn]) nn<-2 
if (dist.train[3]<dist.train[nn]) nn<-3
if (dist.train[4]<dist.train[nn]) nn<-4

corAnswer<-c(corAnswer,corp.train$corA[nn+1])
}

#t_c_3<-select(t_c,c(2,3,4))
#t_c_3<-bind_cols(t_c_3,as.data.frame(corAnswer))


#t_c_itog<-select(t_c,c(2))
#t_c_itog<-bind_cols(t_c_itog,as.data.frame(corAnswer))
#t_c_itog<-slice(t_c_itog,2501:10632)
#write_csv(t_c_itog,"allen2/1.csv")


