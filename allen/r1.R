library(readr)
z<-read_delim("allen/training_set.tsv", delim = "\t",quote = "")

library(tm)
docs <- Corpus(VectorSource(z$question[1:10]))
docs<-tm_map(docs,tolower)
docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removeWords,stopwords("english"))
library(SnowballC)
docs<-tm_map(docs,stemDocument)
docs<-tm_map(docs,stripWhitespace)
docs<-tm_map(docs,PlainTextDocument)


tdm<-TermDocumentMatrix(docs)
colnames(tdm)<-c(rep(1:10))

tdm.dense<-as.matrix(tdm)
tdm.scaled <- tdm.dense / rowSums(tdm.dense)
tdm.dist<-dist(tdm.scaled)

library(cluster)
h<-agnes(tdm.dist, method = "complete")
plot(h, which.plots = 2, main = "", sub = "", xlab = "")

#library(arules)
#docsdissim <- dissimilarity(as.matrix(tdm), method = "cosine")
#h <- hclust(docsdissim, method = "ward")
#plot(h)

