library(caret)
library(dplyr)
library(e1071)
library(NLP)
library(tm)
library(data.table)

ds = read.csv('C:/Users/hugos/Desktop/FS Dataset/Health/kidney-disease-dataset/kidney_disease.csv', 
              header = TRUE)

mycols <- c("id","bp","sg","al","su")

#Convert to numeric
setDT(ds)[, (mycols) := lapply(.SD, as.numeric), .SDcols = mycols]

#Convert classification to logical
data <- ds[,.(bp,sg,al,su,classification = ds$classification == "ckd")]

dataclean <- na.omit(data)

inTrain<-createDataPartition(1:nrow(dataclean),p=0.6,list=FALSE)
train<- dataclean[inTrain]
test <- dataclean[-inTrain]


#trainpredictors<-trainPositive[inTrain,1:4]
#trainLabels<-trainPositive[inTrain,6]

#testPositive<-trainPositive[-inTrain,]
#testPosNeg<-rbind(testPositive,testnegative)

#testpredictors<-testPosNeg[,1:4]
#testLabels<-testPosNeg[,6]

svm.model<-svm(classification ~ bp + sg + al + su, data = train,
               type='one-classification',
               nu=0.10,
               scale=TRUE,
               kernel="radial")

#Perform predictions 
svm.predtrain<-predict(svm.model,train)
svm.predtest<-predict(svm.model,test)

confTrain <- table(Predicted=svm.predtrain,
                   Reference=train$classification[as.integer(names(svm.predtrain))])
confTest <- table(Predicted=svm.predtest,
                  Reference=test$classification[as.integer(names(svm.predtest))])

confusionMatrix(confTest,positive='TRUE')


svm.predtrain<-predict(svm.model,trainpredictors)
svm.predtest<-predict(svm.model,testpredictors)

# confusionMatrixTable<-table(Predicted=svm.pred,Reference=testLabels)
# confusionMatrix(confusionMatrixTable,positive='TRUE')

confTrain <- table(Predicted=svm.predtrain,Reference=trainLabels)
confTest <- table(Predicted=svm.predtest,Reference=testLabels)

confusionMatrix(confTest,positive='TRUE')

print(confTrain)
print(confTest)

#grid
