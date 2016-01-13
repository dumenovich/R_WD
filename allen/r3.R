library(RTextTools)
data("USCongress")


doc_matrix <- create_matrix(USCongress$text, language="english", removeNumbers=TRUE,stemWords=TRUE, removeSparseTerms=.998)
container <- create_container(doc_matrix, USCongress$major, trainSize=1:4000,testSize=4001:4449, virgin=FALSE)

SVM <- train_model(container,"SVM")
SVM_CLASSIFY <- classify_model(container, SVM)

analytics <- create_analytics(container,cbind(SVM_CLASSIFY))
summary(analytics)

# CREATE THE data.frame SUMMARIES
topic_summary <- analytics@label_summary
alg_summary <- analytics@algorithm_summary
ens_summary <-analytics@ensemble_summary
doc_summary <- analytics@document_summary
