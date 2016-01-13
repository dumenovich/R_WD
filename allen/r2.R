library(readr)
library(tm)
library(SnowballC)

# z<-read_lines("allen/obama.txt")

cleanCorpus<-function(crps) {
  crps<-tm_map(crps,tolower)
  crps<-tm_map(crps,removePunctuation)
  crps<-tm_map(crps,removeNumbers)
  crps<-tm_map(crps,removeWords,stopwords("english"))  
  crps<-tm_map(crps,stemDocument)
  crps<-tm_map(crps,stripWhitespace)
  crps<-tm_map(crps,PlainTextDocument)
    
  return (crps)
}

generateTDM<-function(dir.name,dir.path) {
  #s.dir<-sprintf("%s/%s",path,dir.names) 
  
  #z1<-read_lines(s.dir[1])
  #z2<-read_lines(s.dir[2])
  z<-read_lines(dir.name)
  #z<-read_lines(s.dir)
  
  #s.cor<-Corpus(VectorSource(c(z1,z2)))
  s.cor<-Corpus(VectorSource(z))
  
  s.cor.cl<-cleanCorpus(s.cor)
  s.tdm<-TermDocumentMatrix(s.cor.cl)
  
  #s.tdm<-removeSparseTerms(s.tdm,0.7)
  
  #result<-list(name=dir.name,tdm=s.tdm)
}


#dir.names<-c("obama.txt","romney.txt")
path<-"C:/R/R_WD/allen"

#tdm<-lapply(dir.names,generateTDM,dir.path=path)

tdm1<-generateTDM("allen/obama.txt",dir.path=path)
tdm2<-generateTDM("allen/romney.txt",dir.path=path)

tdm<-list(list(name="obama",tdm=tdm1),list(name="romney",tdm=tdm2))

