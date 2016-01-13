library(readr)
library(dplyr)
library(tm)
library(SnowballC)
library(slam)


max_rows<-2500


my_stopwords<-c("best","following","likely","can",  # row_sums>150
                "one","scientists","will",          # row_sums>100
                "student","students","scientist","within","across","like","may","without","outer","per","usually")  # row_sums>10



txt.org<-read_delim("allen2/training_set.tsv",delim = "\t",quote = "")
txt.org.slice<-slice(txt.org,1:max_rows)

corp.org<-Corpus(VectorSource(txt.org.slice$question))

corp.org.clean<-tm_map(corp.org,removeNumbers)
corp.org.clean<-tm_map(corp.org.clean,removePunctuation)
corp.org.clean<-tm_map(corp.org.clean,tolower)
corp.org.clean<-tm_map(corp.org.clean,removeWords,stopwords("english"))
corp.org.clean<-tm_map(corp.org.clean,removeWords,my_stopwords)
corp.org.clean<-tm_map(corp.org.clean,stemDocument)
corp.org.clean<-tm_map(corp.org.clean,stripWhitespace)
corp.org.clean<-tm_map(corp.org.clean,PlainTextDocument)

tdm.orig<-TermDocumentMatrix(corp.org.clean,control = list())
#dtm <- DocumentTermMatrix(crude, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE), stopwords = TRUE))

m.tf<-t(as.matrix(tdm.orig))

#z<-tbl_df(as.data.frame(colnames(m.tf),stringsAsFactors = F))
#colnames(z)<-"term"
#z1<-filter(z,nchar(term)>4)

#m.tf<-m.tf/row_sums(m.tf)

zz<-col_sums(m.tf>0)
z3<-zz


#summary(z3)
#z3[z3>100&z3<120]

# удаляем те термы, которые встречаются в коллекции один раз
#col.less.2<-zz[zz<2]
#m.tf[,(eq1):=NULL]

#ww<-row_sums(mm0>0)
#names(ww)<-seq(1:2500)
#row.less.2<-ww[ww<2]
