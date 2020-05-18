library(readxl)
library(caret)
library(dplyr)
library(e1071)
library(NLP)
library(tm)

ds = read_excel('C:/Users/hugos/Desktop/FS Dataset/Health/covid/dataset.xlsx')

ds_train <- rename(ds, testResult = "SARS-Cov-2 exam result", age = "Patient age quantile" )

#Remover colunas inutiliz?veis              
covid <- subset(ds_train, select = -c(age))


mycols <- c("id","bp","sg","al","su", "bgr","bu","sc", "sod","pot","hemo","pcv","wc","rc")

#Convert to numeric
setDT(ds)[, (mycols) := lapply(.SD, as.numeric), .SDcols = mycols]

#Convert classification to logical
data <- ds[,.(bp,sg,al,su,bgr,bu,sc,sod,pot,hemo,pcv,wc,rc,classification = ds$classification == "ckd")]

dataclean <- na.omit(data)

#Separating train and test
inTrain<-createDataPartition(1:nrow(dataclean),p=0.7,list=FALSE)
train<- dataclean[inTrain]
test <- dataclean[-inTrain]


svm.model<-svm(classification ~ bp+sg+al+su+bgr+bu+sc+sod+pot+hemo+pcv+wc+rc, data = train,
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

print(confTrain)
print(confTest)

